/*
 * Authors: Kent Overstreet <kent.overstreet@gmail.com>
 *	    Gabriel de Perthuis <g2p.code@gmail.com>
 *	    Jacob Malevich <jam@datera.io>
 *
 * GPLv2
 */
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <uuid/uuid.h>

#include "cmds.h"
#include "posix_to_bcachefs.h"
#include "libbcachefs.h"
#include "crypto.h"
#include "libbcachefs/errcode.h"
#include "libbcachefs/opts.h"
#include "libbcachefs/super-io.h"
#include "libbcachefs/util.h"

#include "libbcachefs/darray.h"

#define OPTS						\
x(0,	replicas,		required_argument)	\
x(0,	encrypted,		no_argument)		\
x(0,	no_passphrase,		no_argument)		\
x('L',	fs_label,		required_argument)	\
x('U',	uuid,			required_argument)	\
x(0,	fs_size,		required_argument)	\
x(0,	superblock_size,	required_argument)	\
x(0,	bucket_size,		required_argument)	\
x('l',	label,			required_argument)	\
x(0,	discard,		no_argument)		\
x(0,	data_allowed,		required_argument)	\
x(0,	durability,		required_argument)	\
x(0,	version,		required_argument)	\
x(0,	no_initialize,		no_argument)		\
x(0,	source,			required_argument)	\
x('f',	force,			no_argument)		\
x('q',	quiet,			no_argument)		\
x('v',	verbose,		no_argument)		\
x('h',	help,			no_argument)

static void usage(void)
{
	puts("bcachefs format - create a new bcachefs filesystem on one or more devices\n"
	     "Usage: bcachefs format [OPTION]... <devices>\n"
	     "\n"
	     "Options:");

	bch2_opts_usage(OPT_FORMAT);

	puts(
	     "      --replicas=#            Sets both data and metadata replicas\n"
	     "      --encrypted             Enable whole filesystem encryption (chacha20/poly1305)\n"
	     "      --no_passphrase         Don't encrypt master encryption key\n"
	     "  -L, --fs_label=label\n"
	     "  -U, --uuid=uuid\n"
	     "      --superblock_size=size\n"
	     "      --source=path           Initialize the bcachefs filesystem from this root directory\n"
	     "\n"
	     "Device specific options:");

	bch2_opts_usage(OPT_DEVICE);

	puts("  -l, --label=label           Disk label\n"
	     "\n"
	     "  -f, --force\n"
	     "  -q, --quiet                 Only print errors\n"
	     "  -v, --verbose               Verbose filesystem initialization\n"
	     "  -h, --help                  Display this help and exit\n"
	     "\n"
	     "Device specific options must come before corresponding devices, e.g.\n"
	     "  bcachefs format --label cache /dev/sdb /dev/sdc\n"
	     "\n"
	     "Report bugs to <linux-bcachefs@vger.kernel.org>");
}

enum {
	O_no_opt = 1,
#define x(shortopt, longopt, arg)	O_##longopt,
	OPTS
#undef x
};

#define x(shortopt, longopt, arg) {			\
	.name		= #longopt,			\
	.has_arg	= arg,				\
	.flag		= NULL,				\
	.val		= O_##longopt,			\
},
static const struct option format_opts[] = {
	OPTS
	{ NULL }
};
#undef x

u64 read_flag_list_or_die(char *opt, const char * const list[],
			  const char *msg)
{
	u64 v = bch2_read_flag_list(opt, list);
	if (v == (u64) -1)
		die("Bad %s %s", msg, opt);

	return v;
}

void build_fs(struct bch_fs *c, const char *src_path)
{
	struct copy_fs_state s = {};
	int src_fd = xopen(src_path, O_RDONLY|O_NOATIME);
	struct stat stat = xfstat(src_fd);

	if (!S_ISDIR(stat.st_mode))
		die("%s is not a directory", src_path);

	copy_fs(c, src_fd, src_path, &s);
}

int cmd_format(int argc, char *argv[])
{
	DARRAY(struct dev_opts) devices = { 0 };
	DARRAY(char *) device_paths = { 0 };
	struct format_opts opts	= format_opts_default();
	struct dev_opts dev_opts = dev_opts_default();
	bool force = false, no_passphrase = false, quiet = false, initialize = true, verbose = false;
	bool unconsumed_dev_option = false;
	unsigned v;
	int opt;

	struct bch_opt_strs fs_opt_strs =
		bch2_cmdline_opts_get(&argc, argv, OPT_FORMAT);
	struct bch_opts fs_opts = bch2_parse_opts(fs_opt_strs);

	if (getenv("BCACHEFS_KERNEL_ONLY"))
		initialize = false;

	while ((opt = getopt_long(argc, argv,
				  "-L:U:g:fqhv",
				  format_opts,
				  NULL)) != -1)
		switch (opt) {
		case O_replicas:
			if (kstrtouint(optarg, 10, &v) ||
			    !v ||
			    v > BCH_REPLICAS_MAX)
				die("invalid replicas");

			opt_set(fs_opts, metadata_replicas, v);
			opt_set(fs_opts, data_replicas, v);
			break;
		case O_source:
			opts.source = optarg;
			break;
		case O_encrypted:
			opts.encrypted = true;
			break;
		case O_no_passphrase:
			no_passphrase = true;
			break;
		case O_fs_label:
		case 'L':
			opts.label = optarg;
			break;
		case O_uuid:
		case 'U':
			if (uuid_parse(optarg, opts.uuid.b))
				die("Bad uuid");
			break;
		case O_force:
		case 'f':
			force = true;
			break;
		case O_fs_size:
			if (bch2_strtoull_h(optarg, &dev_opts.size))
				die("invalid filesystem size");
			unconsumed_dev_option = true;
			break;
		case O_superblock_size:
			if (bch2_strtouint_h(optarg, &opts.superblock_size))
				die("invalid filesystem size");

			opts.superblock_size >>= 9;
			break;
		case O_bucket_size:
			if (bch2_strtoull_h(optarg, &dev_opts.bucket_size))
				die("bad bucket_size %s", optarg);
			unconsumed_dev_option = true;
			break;
		case O_label:
		case 'l':
			dev_opts.label = optarg;
			unconsumed_dev_option = true;
			break;
		case O_discard:
			dev_opts.discard = true;
			unconsumed_dev_option = true;
			break;
		case O_data_allowed:
			dev_opts.data_allowed =
				read_flag_list_or_die(optarg,
					__bch2_data_types, "data type");
			unconsumed_dev_option = true;
			break;
		case O_durability:
			if (kstrtouint(optarg, 10, &dev_opts.durability) ||
			    dev_opts.durability > BCH_REPLICAS_MAX)
				die("invalid durability");
			unconsumed_dev_option = true;
			break;
		case O_version:
			if (kstrtouint(optarg, 10, &opts.version))
				die("invalid version");
			break;
		case O_no_initialize:
			initialize = false;
			break;
		case O_no_opt:
			darray_push(&device_paths, optarg);
			dev_opts.path = optarg;
			darray_push(&devices, dev_opts);
			dev_opts.size = 0;
			unconsumed_dev_option = false;
			break;
		case O_quiet:
		case 'q':
			quiet = true;
			break;
		case 'v':
			verbose = true;
		case O_help:
		case 'h':
			usage();
			exit(EXIT_SUCCESS);
			break;
		case '?':
			exit(EXIT_FAILURE);
			break;
		}

	if (unconsumed_dev_option)
		die("Options for devices apply to subsequent devices; got a device option with no device");

	if (opts.version != bcachefs_metadata_version_current)
		initialize = false;

	if (!devices.nr)
		die("Please supply a device");

	if (opts.encrypted && !no_passphrase) {
		opts.passphrase = read_passphrase_twice("Enter passphrase: ");
		initialize = false;
	}

	if (build_compact_filesystem) {
		/* create tmp file */
		/* estimate size of metadata we need? */

		/* append tmp file to list of devices */
		/* set data_allowed appropriately for devices to format */

		/* format */

		new_dev_opts->dawa_allowed = (1U << BCH_DATA_user);
		tmp_dev_opts->data_allowed = ((1U << BCH_DATA_journal)|
					      (1U << BCH_DATA_btree));

		darray_for_each(devices, dev) {
			int ret = open_for_format(dev, force);
			if (ret)
				die("Error opening %s: %s", dev_opts.path, strerror(-ret));
		}

		struct bch_sb *sb =
			bch2_format(fs_opt_strs,
				    fs_opts,
				    opts,
				    devices.data, devices.nr);
		bch2_opt_strs_free(&fs_opt_strs);

		if (!quiet) {
			struct printbuf buf = PRINTBUF;

			buf.human_readable_units = true;

			bch2_sb_to_text(&buf, sb, false, 1 << BCH_SB_FIELD_members_v2);
			printf("%s", buf.buf);

			printbuf_exit(&buf);
		}
		free(sb);

		if (opts.passphrase) {
			memzero_explicit(opts.passphrase, strlen(opts.passphrase));
			free(opts.passphrase);
		}

		darray_exit(&devices);

		struct bch_opts mount_opts = bch2_opts_empty();
		opt_set(mount_opts, verbose, verbose);

		/*
		 * Start the filesystem once, to allocate the journal and create
		 * the root directory:
		 */
		struct bch_fs *c = bch2_fs_open(device_paths.data,
						device_paths.nr,
						mount_opts);
		if (IS_ERR(c))
			die("error opening %s: %s", device_paths.data[0],
			    bch2_err_str(PTR_ERR(c)));

		build_fs(c, opts.source);

		/* set data_allowed |= 1U << BCH_DATA_btree on new device */
		/* now, migrate btrees we want to the new dev */

		for (btrees_that_we_move) {
			ret = bch2_move_btree(c,
					      BBPOS(btree, POS_MIN),
					      BBPOS(btree, SPOS_MAX),
					      your_pred, &op, NULL);
			if (ret)
				die("");
		}

		u64 highest_bucket_used = 0;

		/* truncate image */
		for_each_btree_key_upto(...,
					BTREE_ID_alloc,
					POS(dev_idx, 0),
					POS(dev_idx, U64_MAX),
			if (k.k->type == KEY_TYPE_alloc_v4 &&
			    bkey_s_c_to_alloc_v4(k).v->dirty_sectors)
				highest_bucket_used = k.k->offset;
		);

		/* set ca->mi.nbuckets to highest_bucket_used + 1 */
		/* write super */
		/* truncate file */

		bch2_fs_stop(c);
		return 0;
	}

	darray_for_each(devices, dev) {
		int ret = open_for_format(dev, force);
		if (ret)
			die("Error opening %s: %s", dev_opts.path, strerror(-ret));
	}

	struct bch_sb *sb =
		bch2_format(fs_opt_strs,
			    fs_opts,
			    opts,
			    devices.data, devices.nr);
	bch2_opt_strs_free(&fs_opt_strs);

	if (!quiet) {
		struct printbuf buf = PRINTBUF;

		buf.human_readable_units = true;

		bch2_sb_to_text(&buf, sb, false, 1 << BCH_SB_FIELD_members_v2);
		printf("%s", buf.buf);

		printbuf_exit(&buf);
	}
	free(sb);

	if (opts.passphrase) {
		memzero_explicit(opts.passphrase, strlen(opts.passphrase));
		free(opts.passphrase);
	}

	darray_exit(&devices);

	/* don't skip initialization when we have to build an image from a source */
	if (opts.source && !initialize) {
		printf("Warning: Forcing the initialization because the source flag was supplied\n");
		initialize = 1;
	}

	if (build_compact_filesystem) {
		/* create tmp file */
		/* estimate size of metadata we need? */

		/* append tmp file to list of devices */

		struct bch_opts mount_opts = bch2_opts_empty();

		opt_set(mount_opts, verbose, verbose);

		/*
		 * Start the filesystem once, to allocate the journal and create
		 * the root directory:
		 */
		struct bch_fs *c = bch2_fs_open(device_paths.data,
						device_paths.nr,
						mount_opts);
		if (IS_ERR(c))
			die("error opening %s: %s", device_paths.data[0],
			    bch2_err_str(PTR_ERR(c)));

		if (opts.source)
			build_fs(c, opts.source);

		bch2_fs_stop(c);

	} else if (initialize) {
		struct bch_opts mount_opts = bch2_opts_empty();

		opt_set(mount_opts, verbose, verbose);

		/*
		 * Start the filesystem once, to allocate the journal and create
		 * the root directory:
		 */
		struct bch_fs *c = bch2_fs_open(device_paths.data,
						device_paths.nr,
						mount_opts);
		if (IS_ERR(c))
			die("error opening %s: %s", device_paths.data[0],
			    bch2_err_str(PTR_ERR(c)));

		if (opts.source)
			build_fs(c, opts.source);

		bch2_fs_stop(c);
	}

	darray_exit(&device_paths);

	return 0;
}

static void show_super_usage(void)
{
	puts("bcachefs show-super \n"
	     "Usage: bcachefs show-super [OPTION].. device\n"
	     "\n"
	     "Options:\n"
	     "  -f, --fields=(fields)       list of sections to print\n"
	     "      --field-only=fiel)      print superblock section only, no header\n"
	     "  -l, --layout                print superblock layout\n"
	     "  -h, --help                  display this help and exit\n"
	     "Report bugs to <linux-bcachefs@vger.kernel.org>");
	exit(EXIT_SUCCESS);
}

int cmd_show_super(int argc, char *argv[])
{
	static const struct option longopts[] = {
		{ "fields",			1, NULL, 'f' },
		{ "field-only",			1, NULL, 'F' },
		{ "layout",			0, NULL, 'l' },
		{ "help",			0, NULL, 'h' },
		{ NULL }
	};
	unsigned fields = 0;
	int field_only = -1;
	bool print_layout = false;
	bool print_default_fields = true;
	int opt;

	while ((opt = getopt_long(argc, argv, "f:lh", longopts, NULL)) != -1)
		switch (opt) {
		case 'f':
			fields = !strcmp(optarg, "all")
				? ~0
				: read_flag_list_or_die(optarg,
					bch2_sb_fields, "superblock field");
			print_default_fields = false;
			break;
		case 'F':
			field_only = read_string_list_or_die(optarg,
					bch2_sb_fields, "superblock field");
			print_default_fields = false;
			break;
		case 'l':
			print_layout = true;
			break;
		case 'h':
			show_super_usage();
			break;
		}
	args_shift(optind);

	char *dev = arg_pop();
	if (!dev)
		die("please supply a device");
	if (argc)
		die("too many arguments");

	struct bch_opts opts = bch2_opts_empty();

	opt_set(opts, noexcl,	true);
	opt_set(opts, nochanges, true);

	struct bch_sb_handle sb;
	int ret = bch2_read_super(dev, &opts, &sb);
	if (ret)
		die("Error opening %s: %s", dev, bch2_err_str(ret));

	if (print_default_fields) {
		fields |= bch2_sb_field_get(sb.sb, members_v2)
			? 1 << BCH_SB_FIELD_members_v2
			: 1 << BCH_SB_FIELD_members_v1;
		fields |= 1 << BCH_SB_FIELD_errors;
	}

	struct printbuf buf = PRINTBUF;

	buf.human_readable_units = true;

	if (field_only >= 0) {
		struct bch_sb_field *f = bch2_sb_field_get_id(sb.sb, field_only);

		if (f)
			__bch2_sb_field_to_text(&buf, sb.sb, f);
	} else {
		printbuf_tabstop_push(&buf, 44);

		char *model = fd_to_dev_model(sb.bdev->bd_fd);
		prt_str(&buf, "Device:");
		prt_tab(&buf);
		prt_str(&buf, model);
		prt_newline(&buf);
		free(model);

		bch2_sb_to_text(&buf, sb.sb, print_layout, fields);
	}
	printf("%s", buf.buf);

	bch2_free_super(&sb);
	printbuf_exit(&buf);
	return 0;
}
