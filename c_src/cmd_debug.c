#include <stdio.h>

#include "libbcachefs/bkey_types.h"
#include "libbcachefs/btree_update.h"
#include "libbcachefs/printbuf.h"
#include "libbcachefs/inode.h"

#include "cmds.h"

void write_field(void *base, u64 size, u64 offset, u64 value)
{
	u8 *field8;
	u16 *field16;
	u32 *field32;
	u64 *field64;

	switch (size) {
	case 1:
		field8 = (u8 *) base + offset;
		*field8 = (u8) value;
		break;
	case 2:
		field16 = (u16 *) ((u8 *) base + offset);
		*field16 = (u16) value;
		break;
	case 4:
		field32 = (u32 *) ((u8 *) base + offset);
		*field32 = (u32) value;
		break;
	case 8:
		field64 = (u64 *) ((u8 *) base + offset);
		*field64 = value;
		break;
	default:
		fprintf(stderr, "can't handle size %llu\n", size);
	}
}

int cmd_dump_bkey(struct bch_fs *c, enum btree_id id, struct bpos pos)
{
	struct printbuf buf = PRINTBUF;
	struct btree_trans *trans = bch2_trans_get(c);
	struct btree_iter iter = { NULL };
	int ret = 0;

	bch2_trans_iter_init(trans, &iter, id, pos, BTREE_ITER_ALL_SNAPSHOTS);

	struct bkey_s_c k = bch2_btree_iter_peek(&iter);
	if (!bpos_eq(pos, k.k->p)) {
		printf("no such key\n");
		ret = 1;
		goto out;
	}

	bch2_bkey_val_to_text(&buf, c, k);
	printf("%s\n", buf.buf);

out:
	bch2_trans_iter_exit(trans, &iter);
	bch2_trans_put(trans);

	return ret;
}

int cmd_update_bkey(struct bch_fs *c, struct bkey_update u, struct bpos pos)
{
	struct btree_trans *trans = bch2_trans_get(c);
	struct btree_iter iter = { NULL };
	int ret = 0;

	set_bit(BCH_FS_no_invalid_checks, &c->flags);

	if (!strcmp(u.bkey, "bch_inode_unpacked")) {
		bch2_trans_iter_init(trans, &iter, BTREE_ID_inodes, pos,
				     BTREE_ITER_ALL_SNAPSHOTS);

		struct bkey_s_c k = bch2_btree_iter_peek(&iter);
		if (bkey_err(k)) {
			// TODO: is this proper error handling?
			printf("error getting key: %s\n", bch2_err_str(PTR_ERR(k.k)));
			ret = 1;
			goto out;
		}
		struct bch_inode_unpacked inode;
		// TODO: check error
		bch2_inode_unpack(k, &inode);

		write_field(&inode, u.size, u.offset, u.value);

		ret = bch2_inode_write(trans, &iter, &inode) ?:
			  bch2_trans_commit(trans, NULL, NULL, BCH_TRANS_COMMIT_no_invalid_checks);
		if (ret != 0) {
			printf("ret = %s (%d)\n", bch2_err_str(ret), ret);
		}
	} else {
		// TODO can this return an error?
		struct bkey_i *k = bch2_bkey_get_mut(trans, &iter, u.id, pos,
						     BTREE_ITER_ALL_SNAPSHOTS);
		// TODO: check bkey type to confirm it matches specified type?
		bch2_trans_unlock(trans);

		write_field(&k->v, u.size, u.offset, u.value);

		ret = bch2_btree_insert(c, u.id, k, NULL, BCH_TRANS_COMMIT_no_invalid_checks);
		if (ret != 0) {
			printf("ret = %s (%d)\n", bch2_err_str(ret), ret);
		}
	}

out:
	bch2_trans_iter_exit(trans, &iter);
	bch2_trans_put(trans);

	return ret;
}
