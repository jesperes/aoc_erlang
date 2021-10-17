#include <openssl/md5.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#define TO_HEXB(X) (char)(((X) <= 9) ? ((X) + 48) : ((X) + 87))

static ERL_NIF_TERM digest_to_hexstring(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  ErlNifBinary in_binary;

  uint8_t *outbuf = enif_make_new_binary(env, 32, &term);

  if (enif_inspect_binary(env, argv[0], &in_binary)) {
    if (in_binary.size != 16) {
      enif_make_badarg(env);
      return term;
    }

    uint8_t *digest = in_binary.data;
    for (int j = 0; j < 16; j++) {
      outbuf[j * 2] = TO_HEXB((digest[j] & 0xf0) >> 4);
      outbuf[j * 2 + 1] = TO_HEXB(digest[j] & 0x0f);
    }

    return term;
  } else {
    enif_make_badarg(env);
    return term;
  }
  return term;
}

static ErlNifFunc nif_funcs[] = {
    {"digest_to_hexstring", 1, digest_to_hexstring}};

ERL_NIF_INIT(aoc_nifs, nif_funcs, NULL, NULL, NULL, NULL);
