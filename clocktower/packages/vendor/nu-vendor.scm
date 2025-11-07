;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (clocktower packages vendor nu-vendor)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler32-1.2.0
  (crate-source "adler32" "1.2.0"
                "0d7jq7jsjyhsgbhnfq5fvrlh9j0i9g1fqrl2735ibv5f75yjgqda"))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-alloc-no-stdlib-2.0.4
  (crate-source "alloc-no-stdlib" "2.0.4"
                "1cy6r2sfv5y5cigv86vms7n5nlwhx1rbyxwcraqnmm1rxiib2yyc"))

(define rust-alloc-stdlib-0.2.2
  (crate-source "alloc-stdlib" "0.2.2"
                "1kkfbld20ab4165p29v172h8g0wvq8i06z8vnng14whw0isq5ywl"))

(define rust-alloca-0.4.0
  (crate-source "alloca" "0.4.0"
                "1x6p4387rz6j7h342kp3b7bgvqzyl9mibf959pkfk9xflrgd19z5"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-alphanumeric-sort-1.5.3
  (crate-source "alphanumeric-sort" "1.5.3"
                "13vyx3cqpylvc0md4563rd42b7dvk3fv4wny0kpcc48gy72n0z6n"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-ansi-str-0.9.0
  (crate-source "ansi-str" "0.9.0"
                "03c9j3870slj40lkdkrpav2p4kig2f1g6x42n8267x397d2y2386"))

(define rust-ansitok-0.3.0
  (crate-source "ansitok" "0.3.0"
                "1vjrlvmwrq5v72rcmfqhdyspvabcz5mx531am7q6071gikmara60"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-anyhow-1.0.94
  (crate-source "anyhow" "1.0.94"
                "1xqz3j4h3dxiqi37k8dwl5cc2sb3rlzy7rywfqiblf7g52h07zf1"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arboard-3.4.1
  (crate-source "arboard" "3.4.1"
                "1x2p8dfhzm3w0cpw81ab2rbyzvkzqs9g66xcakq4y0fd2v5rq2fz"))

(define rust-argminmax-0.6.3
  (crate-source "argminmax" "0.6.3"
                "0rcy6nq86wqwfbqpxzpdq8lpmx76c66ifd7fg7nd5j0slh83vwbh"))

(define rust-array-init-cursor-0.2.1
  (crate-source "array-init-cursor" "0.2.1"
                "1hqzgcw4930bp8gw2qy10nfyw7c3kwgwaf5yd2klw7ad487zwlgd"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-assert-cmd-2.0.16
  (crate-source "assert_cmd" "2.0.16"
                "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))

(define rust-assert-json-diff-2.0.2
  (crate-source "assert-json-diff" "2.0.2"
                "04mg3w0rh3schpla51l18362hsirl23q93aisws2irrj32wg5r27"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-stream-0.3.6
  (crate-source "async-stream" "0.3.6"
                "0xl4zqncrdmw2g6241wgr11dxdg4h7byy6bz3l6si03qyfk72nhb"))

(define rust-async-stream-impl-0.3.6
  (crate-source "async-stream-impl" "0.3.6"
                "0kaplfb5axsvf1gfs2gk6c4zx6zcsns0yf3ssk7iwni7bphlvhn7"))

(define rust-async-trait-0.1.83
  (crate-source "async-trait" "0.1.83"
                "1p8q8gm4fv2fdka8hwy2w3f8df7p5inixqi7rlmbnky3wmysw73j"))

(define rust-atoi-simd-0.16.0
  (crate-source "atoi_simd" "0.16.0"
                "1sfvqhx7845j9629qhba9b7p71jhkd28agbqxcmi228jjvlgk427"))

(define rust-atomic-0.6.0
  (crate-source "atomic" "0.6.0"
                "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-avro-schema-0.3.0
  (crate-source "avro-schema" "0.3.0"
                "1gbvciwvi2isa6qanbzi4lbqzzgvhdlzjyzlsa29dflsndaiha5m"))

(define rust-aws-config-1.5.10
  (crate-source "aws-config" "1.5.10"
                "09599cbh1k266ajz9g2hv9p6v5cqix3008hsgrbxi38y6jmayjcv"))

(define rust-aws-credential-types-1.2.1
  (crate-source "aws-credential-types" "1.2.1"
                "1njlj7gyyxpf5v1a29gv1h7k216ii986h8hkk05ccpyb2nvgds30"))

(define rust-aws-runtime-1.4.3
  (crate-source "aws-runtime" "1.4.3"
                "0s14fv2zkp9kcwpskhwkfg7hs3yk9bky5831jm6ich55b82mq3d1"))

(define rust-aws-sdk-sso-1.49.0
  (crate-source "aws-sdk-sso" "1.49.0"
                "190c85msddhq03fqiccqsba9g1b5m6s0j0f6iln1g4nsm5274rq9"))

(define rust-aws-sdk-ssooidc-1.50.0
  (crate-source "aws-sdk-ssooidc" "1.50.0"
                "1vpppvpwa7ah3zlsg0wiziqg4rczqnfdaymf684x2fxvm3rs5zl1"))

(define rust-aws-sdk-sts-1.50.0
  (crate-source "aws-sdk-sts" "1.50.0"
                "1md7qdyra4r2lzi82xxlbj8kivgqyx9fyzbjg7f4dhkaybjm9nka"))

(define rust-aws-sigv4-1.2.5
  (crate-source "aws-sigv4" "1.2.5"
                "1ccr2ihqlj2ffpsplqz4hwsrxik8hf78xfxzc3kkn9cg1lm786an"))

(define rust-aws-smithy-async-1.2.1
  (crate-source "aws-smithy-async" "1.2.1"
                "131pl4pagdv6zyj1iidgfx370vlrg0gkdwdma7fnv53zx730n8k2"))

(define rust-aws-smithy-http-0.60.11
  (crate-source "aws-smithy-http" "0.60.11"
                "1dksgm36hch21s1fm7vskfip4gsmzq11qc3fjxyx1f66zplc72sw"))

(define rust-aws-smithy-json-0.60.7
  (crate-source "aws-smithy-json" "0.60.7"
                "1mpjhs26q22jglmm85rnb79s0681jqlx2wrlmn6lc2ggd6adz0s6"))

(define rust-aws-smithy-query-0.60.7
  (crate-source "aws-smithy-query" "0.60.7"
                "1fv43jv9p3z9c4x1154zrlrmhcy5rs4jdr2jfg5s3s1zxcfddyzj"))

(define rust-aws-smithy-runtime-1.7.3
  (crate-source "aws-smithy-runtime" "1.7.3"
                "1qkbsz5v3p2hrbd6islvhsh5yk6dsxl8pz1is5qxh7x97w3bsa5y"))

(define rust-aws-smithy-runtime-api-1.7.3
  (crate-source "aws-smithy-runtime-api" "1.7.3"
                "1kc5nmx0sjlry0bx55ing3zvg6h6z0ph6haggck2p0bsljb545lj"))

(define rust-aws-smithy-types-1.2.9
  (crate-source "aws-smithy-types" "1.2.9"
                "041m7mgfqdnf7ssmxl4x8d830fddsfc7vqkgh39maz9s5fir9gag"))

(define rust-aws-smithy-xml-0.60.9
  (crate-source "aws-smithy-xml" "0.60.9"
                "1z280vwfd49scifxqhnhp383a759ngw757sivk9h19vsh9k022xb"))

(define rust-aws-types-1.3.3
  (crate-source "aws-types" "1.3.3"
                "1vrzq4d9zv2dwp0gcvjln4n7f6w0hj4zsa8865snc7j47qdvj8aj"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-backtrace-ext-0.2.1
  (crate-source "backtrace-ext" "0.2.1"
                "0l4xacjnx4jrn9k14xbs2swks018mviq03sp7c1gn62apviywysk"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64-simd-0.8.0
  (crate-source "base64-simd" "0.8.0"
                "15cihnjqpxy0h7llpk816czyp5z613yrvsivw9i8f5vkivkvp6ik"))

(define rust-base64ct-1.8.0
  (crate-source "base64ct" "1.8.0"
                "1fj4vc6ghy3j1120r7dwn4xw90crfy46b448g5pm9w6an13qn92m"))

(define rust-bigdecimal-0.4.8
  (crate-source "bigdecimal" "0.4.8"
                "04q0d4f9k3czy6ynydd8gspig046f85kav6cgh1266vsmclg48hs"))

(define rust-bincode-2.0.1
  (crate-source "bincode" "2.0.1"
                "0h5pxp3dqkigjwy926a03sl69n9wv7aq4142a20kw9lhn3bzbsin"))

(define rust-bincode-derive-2.0.1
  (crate-source "bincode_derive" "2.0.1"
                "029wmh26hq3hhs1gq629y0frn2pkl7ld061rk23fji8g8jd715dz"))

(define rust-bindgen-0.70.1
  (crate-source "bindgen" "0.70.1"
                "0vyf0jp6apcy9kjyz4s8vldj0xqycnbzb6zv3skkwiqdi3nqz7gl"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-blake3-1.8.2
  (crate-source "blake3" "1.8.2"
                "1854x65zmjh9w9cfhyyyg0wmm2k5d87l13l4m7y40ajbkslam21q"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-boxcar-0.2.13
  (crate-source "boxcar" "0.2.13"
                "1akbyh04920dm0h4lyyklasdpwjar6lffzwc18rpgdkrr5dr5i16"))

(define rust-bracoxide-0.1.6
  (crate-source "bracoxide" "0.1.6"
                "1g8h33334hfajh8qpnaifdb8pjk2ilj3f9jsn8mk68m18m2b4wim"))

(define rust-brotli-8.0.2
  (crate-source "brotli" "8.0.2"
                "0q25r00z3gm5wzvv4vfxvlx5zjb8i4jwyznrvdcp7abs7ihbkn2b"))

(define rust-brotli-decompressor-5.0.0
  (crate-source "brotli-decompressor" "5.0.0"
                "00yyswj1rj20ma4wr4wcci4r9ywlgvxa87nqsv5rik5y588vhjw7"))

(define rust-bstr-1.11.1
  (crate-source "bstr" "1.11.1"
                "1j32ghvgwvc40bc6g38ximk52dv1xfknkzfmdwpgjnrsd1yk0skq"))

(define rust-buf-trait-0.4.1
  (crate-source "buf-trait" "0.4.1"
                "1d0pxqvynln4p58n7ajl95fk0x772xvgxjpsqgb77h78f33szsi1"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bytecount-0.6.8
  (crate-source "bytecount" "0.6.8"
                "1klqfjwn41fwmcqw4z03v6i4imgrf7lmf3b5s9v74hxir8hrps2w"))

(define rust-bytemuck-1.23.0
  (crate-source "bytemuck" "1.23.0"
                "134mywp7bqm252c851h3bny75g0l5kw99738a1mkcjyf07pscd4i"))

(define rust-bytemuck-derive-1.8.0
  (crate-source "bytemuck_derive" "1.8.0"
                "1v5r33dgl12rqbvh440fdjxmxxr49qpzmg6vpw5jzdbcjk6w7z5w"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-bytes-1.10.0
  (crate-source "bytes" "1.10.0"
                "1ybcmdrlxrsrn7lnl0xrjg10j7zb4r01jjs5b2sqhrcwh62aq7gn"))

(define rust-bytes-utils-0.1.4
  (crate-source "bytes-utils" "0.1.4"
                "0dcd0lxfpj367j9nwm7izj4mkib3slg61rg4wqmpw0kvfnlf7bvx"))

(define rust-bytesize-2.1.0
  (crate-source "bytesize" "2.1.0"
                "0bw287slwwybbv4gg2pyrr2zyiwwaaz9w0g90fi9q27h7jp39i7m"))

(define rust-byteyarn-0.5.1
  (crate-source "byteyarn" "0.5.1"
                "0yygyhncfij3skkb3824wr1xn1f47p0y09c5kyjmx8b8ck952gmr"))

(define rust-calamine-0.28.0
  (crate-source "calamine" "0.28.0"
                "06lzz5ik56cz6q8hm5yx0w3gzlgf7m5chvhaifkpkrwxwwc2mq0m"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-castaway-0.2.3
  (crate-source "castaway" "0.2.3"
                "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.1.1
  (crate-source "cfg_aliases" "0.1.1"
                "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-chardetng-0.1.17
  (crate-source "chardetng" "0.1.17"
                "1spikjcnblwa5n1nnk46fxkwn86yfiqxgs47h4yaw23vbfvg1f0l"))

(define rust-charset-0.1.5
  (crate-source "charset" "0.1.5"
                "0zkwcw525qwcqsdf74l9d2r6m69yxfxb4kgywp3q9fklgjq2gygi"))

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-chrono-humanize-0.2.3
  (crate-source "chrono-humanize" "0.2.3"
                "0fq25fcdqd7s39dx81hq123210q4lpcbjdz82jl2fy6jnkk2g5kr"))

(define rust-chrono-tz-0.10.0
  (crate-source "chrono-tz" "0.10.0"
                "1dnkmhlf8jfxskrzir0y6fdbpf8mqnaz5igql9cklwh0dl2dhvfd"))

(define rust-chrono-tz-build-0.4.0
  (crate-source "chrono-tz-build" "0.4.0"
                "19qh9c3v7v4czf8z8vrhdy4jmkc6nyzd4svlwwlm493sswsflkz9"))

(define rust-chumsky-0.9.3
  (crate-source "chumsky" "0.9.3"
                "1jcnafc8rjfs1al08gqzyn0kpbaizgdwrd0ajqafspd18ikxdswf"))

(define rust-clang-sys-1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.23
  (crate-source "clap" "4.5.23"
                "110cf0i9fmkfqzqhi1h8za9y0vnr5rwhy3wmv1p0rcgp5vnffd9i"))

(define rust-clap-builder-4.5.23
  (crate-source "clap_builder" "4.5.23"
                "0f28rgc09kdgfq1hgg1bb1ydaw243w6dwyw74syz439k6b32yn1h"))

(define rust-clap-derive-4.5.18
  (crate-source "clap_derive" "4.5.18"
                "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clipboard-win-5.4.0
  (crate-source "clipboard-win" "5.4.0"
                "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))

(define rust-codepage-0.1.2
  (crate-source "codepage" "0.1.2"
                "1d0qr4wqc4yrab7halsa3r6akb2i2bk2cqr04vl8m0n23c38vxj8"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colorz-1.1.4
  (crate-source "colorz" "1.1.4"
                "0yq6wvrajh73b9hwjr03brc2znhr1x1nym6bd5ry68c8g72kgsvc"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-comfy-table-7.1.3
  (crate-source "comfy-table" "7.1.3"
                "1nd4ns4vimypk554vqjww3iq14mdjbaawn5q1jl6w9j3nvknbw94"))

(define rust-compact-str-0.8.0
  (crate-source "compact_str" "0.8.0"
                "0i7nscqyc2szdqpzfqnakcih8mq1f74g4c8b2q9f9cnsdnhw6l30"))

(define rust-compact-str-0.9.0
  (crate-source "compact_str" "0.9.0"
                "0ykhh2scg32lmzxak107pmby6fmnz7qbhsi9i8g9iknfl4ji7nrz"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-console-0.15.8
  (crate-source "console" "0.15.8"
                "1sz4nl9nz8pkmapqni6py7jxzi7nzqjxzb3ya4kxvmkb0zy867qf"))

(define rust-console-0.16.1
  (crate-source "console" "0.16.1"
                "1x4x6vfi1s55nbr4i77b9r87s213h46lq396sij9fkmidqx78c5l"))

(define rust-const-format-0.2.34
  (crate-source "const_format" "0.2.34"
                "1pb3vx4k0bl3cy45fmba36hzds1jhkr8y9k3j5nnvm4abjb9fvqj"))

(define rust-const-format-proc-macros-0.2.34
  (crate-source "const_format_proc_macros" "0.2.34"
                "0i3pxxcl4xvwq4mlfg3csb4j0n6v0mhj07p6yk0vlvdirznc4mqx"))

(define rust-const-random-0.1.18
  (crate-source "const-random" "0.1.18"
                "0n8kqz3y82ks8znvz1mxn3a9hadca3amzf33gmi6dc3lzs103q47"))

(define rust-const-random-macro-0.1.16
  (crate-source "const-random-macro" "0.1.16"
                "03iram4ijjjq9j5a7hbnmdngj8935wbsd0f5bm8yw2hblbr3kn7r"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

(define rust-cookie-0.18.1
  (crate-source "cookie" "0.18.1"
                "0iy749flficrlvgr3hjmf3igr738lk81n5akzf4ym4cs6cxg7pjd"))

(define rust-cookie-store-0.21.1
  (crate-source "cookie_store" "0.21.1"
                "1y9ydb52bcd1zc7r0mppy8c8l541p459a006xr0m52pq50c91b1f"))

(define rust-core-foundation-0.10.0
  (crate-source "core-foundation" "0.10.0"
                "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-cpufeatures-0.2.16
  (crate-source "cpufeatures" "0.2.16"
                "1hy466fkhxjbb16i7na95wz8yr14d0kd578pwzj5lbkz14jh5f0n"))

(define rust-crc-2.1.0
  (crate-source "crc" "2.1.0"
                "08qfahmly0n5j27g1vkqx9s6mxhm8k4dsp61ykskazyabdlrmz29"))

(define rust-crc-catalog-1.1.1
  (crate-source "crc-catalog" "1.1.1"
                "00qlxgzg15fnyx6nwviibz94rjw803l2avi2k3shjfx0dnsyvbnc"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.5
  (crate-source "crossbeam-deque" "0.8.5"
                "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.11
  (crate-source "crossbeam-queue" "0.3.11"
                "0d8y8y3z48r9javzj67v3p2yfswd278myz1j9vzc4sp7snslc0yz"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.28.1
  (crate-source "crossterm" "0.28.1"
                "1im9vs6fvkql0sr378dfr4wdm1rrkrvr22v4i8byz05k1dd9b7c2"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crunchy-0.2.2
  (crate-source "crunchy" "0.2.2"
                "1dx9mypwd5mpfbbajm78xcrg5lirqk7934ik980mmaffg3hdm0bs"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-cssparser-0.35.0
  (crate-source "cssparser" "0.35.0"
                "1am2mj4rddlbmi08drk7gv9m8vw47zgicld48kwp451sfgfix42f"))

(define rust-cssparser-macros-0.6.1
  (crate-source "cssparser-macros" "0.6.1"
                "0cfkzj60avrnskdmaf7f8zw6pp3di4ylplk455zrzaf19ax8id8k"))

(define rust-csv-1.3.1
  (crate-source "csv" "1.3.1"
                "1bzxgbbhy27flcyafxbj7f1hbn7b8wac04ijfgj34ry9m61lip5c"))

(define rust-csv-core-0.1.11
  (crate-source "csv-core" "0.1.11"
                "0w7s7qa60xb054rqddpyg53xq2b29sf3rbhcl8sbdx02g4yjpyjy"))

(define rust-ctrlc-3.4.5
  (crate-source "ctrlc" "3.4.5"
                "1lqdhyl8csq8l2011g4w3wjps84w2cmwfn7jhx79ngrgm45apvlh"))

(define rust-curl-0.4.47
  (crate-source "curl" "0.4.47"
                "0rcjdrl35xs8l5v3wv6q5z37hjw3r5bvmbb09pqmhaxyl49lvyyr"))

(define rust-curl-sys-0.4.78+curl-8.11.0
  ;; TODO: Check bundled sources.
  (crate-source "curl-sys" "0.4.78+curl-8.11.0"
                "1bqyh8rlwhwj937d1md5chpg56ch8mncyldf26b7iiy5861pdv4f"))

(define rust-darling-0.20.10
  (crate-source "darling" "0.20.10"
                "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))

(define rust-darling-0.21.0
  (crate-source "darling" "0.21.0"
                "1s9gagl4z88r9gi978vjs692wkf567h7ck5y0j9kvynm3z5lm757"))

(define rust-darling-core-0.20.10
  (crate-source "darling_core" "0.20.10"
                "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))

(define rust-darling-core-0.21.0
  (crate-source "darling_core" "0.21.0"
                "07bagpvji6vrqqj4i4jcxjm0709n9m6vh44rc1cync5g1plmv1vl"))

(define rust-darling-macro-0.20.10
  (crate-source "darling_macro" "0.20.10"
                "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))

(define rust-darling-macro-0.21.0
  (crate-source "darling_macro" "0.21.0"
                "0im1qx22wqiypkp5byx3vrf22jznwpw6alnqghbgjpbxcxhqx7z7"))

(define rust-data-encoding-2.9.0
  (crate-source "data-encoding" "2.9.0"
                "0xm46371aw613ghc12ay4vsnn49hpcmcwlijnqy8lbp2bpd308ra"))

(define rust-der-0.7.10
  (crate-source "der" "0.7.10"
                "1jyxacyxdx6mxbkfw99jz59dzvcd9k17rq01a7xvn1dr6wl87hg7"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-derive-arbitrary-1.4.1
  (crate-source "derive_arbitrary" "1.4.1"
                "000839h4mbgs65x1f8540kbjk2ifw68c4d8r5b9f7q0jv4d2qm1h"))

(define rust-derive-more-2.0.1
  (crate-source "derive_more" "2.0.1"
                "0y3n97cc7rsvgnj211p92y1ppzh6jzvq5kvk6340ghkhfp7l4ch9"))

(define rust-derive-more-impl-2.0.1
  (crate-source "derive_more-impl" "2.0.1"
                "1wqxcb7d5lzvpplz9szp4rwy1r23f5wmixz0zd2vcjscqknji9mx"))

(define rust-derive-new-0.6.0
  (crate-source "derive-new" "0.6.0"
                "1b8jv6jx0b8jgkz9kmz0ciqmnf74xkk0mmvkb5z1c87932kdwl6i"))

(define rust-devicons-0.6.12
  (crate-source "devicons" "0.6.12"
                "0jwh0g72rfkpbsm16rxb47y3ylmr47wwx3cmbbflzkrhygi4f3l3"))

(define rust-dialoguer-0.11.0
  (crate-source "dialoguer" "0.11.0"
                "1pl0744wwr97kp8qnaybzgrfwk66qakzq0i1qrxl03vpbn0cx2v5"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dirs-6.0.0
  (crate-source "dirs" "6.0.0"
                "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))

(define rust-dirs-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-dlv-list-0.5.2
  (crate-source "dlv-list" "0.5.2"
                "0pqvrinxzdz7bpy4a3p450h8krns3bd0mc3w0qqvm03l2kskj824"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-doctest-file-1.0.0
  (crate-source "doctest-file" "1.0.0"
                "0qkmnrsx2kszm58wxyry63bs35msj9chdb6jlh54a8cdwaiizj5a"))

(define rust-document-features-0.2.11
  (crate-source "document-features" "0.2.11"
                "0pdhpbz687fk2rkgz45yy3gvbhlxliwb7g1lj3jbx1f1qr89n94m"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dtoa-1.0.9
  (crate-source "dtoa" "1.0.9"
                "0lv6zzgrd3hfh83n9jqhzz8645729hv1wclag8zw4dbmx3w2pfyw"))

(define rust-dtoa-short-0.3.5
  (crate-source "dtoa-short" "0.3.5"
                "11rwnkgql5jilsmwxpx6hjzkgyrbdmx1d71s0jyrjqm5nski25fd"))

(define rust-dtparse-2.0.1
  (crate-source "dtparse" "2.0.1"
                "1mqz4164mc4xyq73c22wf900v8cn4sy63nalrkr5mlr614y41yr3"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dyn-clone-1.0.17
  (crate-source "dyn-clone" "1.0.17"
                "09cig7dgg6jnqa10p4233nd8wllbjf4ffsw7wj0m4lwa5w3z0vhd"))

(define rust-ego-tree-0.10.0
  (crate-source "ego-tree" "0.10.0"
                "1n2csy99chk5v5vzjl0ff79vxpxhl76xmcb3aj6brrzzipmjz5xj"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-eml-parser-0.1.4
  (crate-source "eml-parser" "0.1.4"
                "0bhgx1i952g2v7w0icnqkylvhfiyb2am2mbw89v8zq0jz0aqvnx7"))

(define rust-encode-unicode-0.3.6
  (crate-source "encode_unicode" "0.3.6"
                "07w3vzrhxh9lpjgsg2y5bwzfar2aq35mdznvcp3zjl0ssj7d4mx3"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-env-filter-0.1.2
  (crate-source "env_filter" "0.1.2"
                "1avnnd60ig6q5ixxxbwicwkxpqjg8bl9x7qn8c7wbvkcvb794b2g"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.11.5
  (crate-source "env_logger" "0.11.5"
                "13812wq68kybv2vj6rpnhbl7ammlhggcb7vq68bkichzp4cscgz1"))

(define rust-env-logger-0.8.4
  (crate-source "env_logger" "0.8.4"
                "1qzw8g11dbdfi7ixm44ldykwcqsxqkh8vx5cgpd88zmclgz8g4d1"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-erased-serde-0.4.5
  (crate-source "erased-serde" "0.4.5"
                "13dirfj9972nvk05b20w3xyn3xp1j6qyfp9avhksnkxbcnfkiqi4"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-error-code-3.3.1
  (crate-source "error-code" "3.3.1"
                "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5"))

(define rust-etcetera-0.10.0
  (crate-source "etcetera" "0.10.0"
                "1rka6bskn93pdhx32xaagr147q95z5bnz7ym5xr85jw00wyv3ir6"))

(define rust-ethnum-1.5.0
  (crate-source "ethnum" "1.5.0"
                "0b68ngvisb0d40vc6h30zlhghbb3mc8wlxjbf8gnmavk1dca435r"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fancy-regex-0.16.1
  (crate-source "fancy-regex" "0.16.1"
                "0z4lv4madmsl2536smbm6jcqiqpc7ail1d57an1wwjj62pnca15z"))

(define rust-fast-float2-0.2.3
  (crate-source "fast-float2" "0.2.3"
                "0mbadcgq221clfpihsfiahizfsgfwk8n3dbgi1fd48vlbi65dszq"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fd-lock-4.0.2
  (crate-source "fd-lock" "4.0.2"
                "0ixrsd19k2cpl773p9hd0yk3hac684d9aphbxy0jq9q64bd6hmvy"))

(define rust-file-id-0.2.2
  (crate-source "file-id" "0.2.2"
                "0dmylm34z6g8cg3b60sc6bk9v5wv9930vyx9wgcdpjpgpfwh9jbb"))

(define rust-filesize-0.2.0
  (crate-source "filesize" "0.2.0"
                "0hvx4dfnara3a2dnhb9ci5bmm1m8s44h9l61s5djwkjx87i43mqj"))

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-float-cmp-0.10.0
  (crate-source "float-cmp" "0.10.0"
                "1n760i3nxd2x0zc7fkxkg3vhvdyfbvzngna006cl9s9jacaz775h"))

(define rust-fluent-0.17.0
  (crate-source "fluent" "0.17.0"
                "0xq4cxw4mkdh1k9i5w850sky0m41la8sm6nbpw76n3f5lbascdw1"))

(define rust-fluent-bundle-0.16.0
  (crate-source "fluent-bundle" "0.16.0"
                "1x1v8bmym6x9pl87f82lbzwlc84kdn0lgcwi73ki2mwgj6w3q801"))

(define rust-fluent-langneg-0.13.0
  (crate-source "fluent-langneg" "0.13.0"
                "152yxplc11vmxkslvmaqak9x86xnavnhdqyhrh38ym37jscd0jic"))

(define rust-fluent-syntax-0.12.0
  (crate-source "fluent-syntax" "0.12.0"
                "1661sp6kl268n445x7jjhnbkgiaa1xcpyryq0i6iiz9zqn3x5w2l"))

(define rust-fluent-uri-0.1.4
  (crate-source "fluent-uri" "0.1.4"
                "03ah2qajw5l1zbc81kh1n8g7n24mfxbg6vqyv9ixipg1vglh9iqp"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-fs4-0.13.1
  (crate-source "fs4" "0.13.1"
                "1m0y2kmwzifkrivw7gjav0km5s9agaiv324yrq424rgpi15y6h46"))

(define rust-fsevent-sys-4.1.0
  ;; TODO: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-futf-0.1.5
  (crate-source "futf" "0.1.5"
                "0hvqk2r7v4fnc34hvc3vkri89gn52d5m9ihygmwn75l1hhp0whnz"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-fxhash-0.2.1
  (crate-source "fxhash" "0.2.1"
                "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))

(define rust-generator-0.8.7
  (crate-source "generator" "0.8.7"
                "18p5rjx7vbcz6jw6c6qdzpl2wkhr5mfcqdc60g0skqp372jq6lb0"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.3.1
  (crate-source "getrandom" "0.3.1"
                "1y154yzby383p63ndw6zpfm0fz3vf6c0zdwc7df6vkl150wrr923"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-git2-0.20.0
  (crate-source "git2" "0.20.0"
                "1zwav0r76njd9chqxh7wj4r4zfn08nzsisrg05liyd6cjf4piniz"))

(define rust-gjson-0.8.1
  (crate-source "gjson" "0.8.1"
                "1dlp19c42f3qlzckxqwvc6svhfc3hdpg7x95cl5d6k9rfv0kql23"))

(define rust-glob-0.3.1
  (crate-source "glob" "0.3.1"
                "16zca52nglanv23q5qrwd5jinw3d3as5ylya6y1pbx47vkxvrynj"))

(define rust-glob-match-0.2.1
  (crate-source "glob-match" "0.2.1"
                "178bjn684dd50px9n8lwa72fn94566d9wmcp86m9h8a17d8ck1cr"))

(define rust-goblin-0.7.1
  (crate-source "goblin" "0.7.1"
                "0d11fk9bdxzf228xpr8v6d6a01dib00khjg5bldk9kf2d51inz7j"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.4.7
  (crate-source "h2" "0.4.7"
                "0bljg66n2x3c5yzbi12v2jfcj77hb35rjq0gq21x0d6n52bjgbnc"))

(define rust-halfbrown-0.3.0
  (crate-source "halfbrown" "0.3.0"
                "1airrrxwmqgnpl02cy0xwhscvlnv74qdjwxnpf0d23zpdmf3hb5a"))

(define rust-hash32-0.3.1
  (crate-source "hash32" "0.3.1"
                "01h68z8qi5gl9lnr17nz10lay8wjiidyjdyd60kqx8ibj090pmj7"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-heapless-0.9.1
  (crate-source "heapless" "0.9.1"
                "19ddqwmnhi08ia8wnkbqaim4glyg8sl32xfbpn7nhr4f6ddcvvdi"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.9
  (crate-source "home" "0.5.9"
                "19grxyg35rqfd802pcc9ys1q3lafzlcjcv2pl2s5q8xpyr5kblg3"))

(define rust-html5ever-0.27.0
  (crate-source "html5ever" "0.27.0"
                "1m24sbpk572f5qhhkj4kkxvsd64rn968s0vxwvqlds76w2pp2dy1"))

(define rust-html5ever-0.35.0
  (crate-source "html5ever" "0.35.0"
                "1m4yajw7slxqn0x3zdh3i9qlhb03vgdf2pq3la3l8rjbyz15inam"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.2
  (crate-source "http-body-util" "0.1.2"
                "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))

(define rust-httparse-1.9.5
  (crate-source "httparse" "1.9.5"
                "0ip9v8m9lvgvq1lznl31wvn0ch1v254na7lhid9p29yx9rbx6wbx"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-human-date-parser-0.3.1
  (crate-source "human-date-parser" "0.3.1"
                "03hiqw8yxsi6ps0fzmykqghc08pszsjfjap57ccckcp4dp2q6vs0"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-hyper-0.14.31
  (crate-source "hyper" "0.14.31"
                "11bf6mqcpzi0x2758p7q9zk3m877avzpbiw8nx8v2dd3iwp3024c"))

(define rust-hyper-1.5.1
  (crate-source "hyper" "1.5.1"
                "07s87id0566m2p5dc5q6nqmxz5r8drqd81b7w4q44djgxwkqi0cp"))

(define rust-hyper-rustls-0.24.2
  (crate-source "hyper-rustls" "0.24.2"
                "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))

(define rust-hyper-rustls-0.27.3
  (crate-source "hyper-rustls" "0.27.3"
                "0cxkph8hsmbz693a8ih2ciy7h0xbac844rpm981y6c0iqfsxpbq8"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.10
  (crate-source "hyper-util" "0.1.10"
                "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz"))

(define rust-iana-time-zone-0.1.61
  (crate-source "iana-time-zone" "0.1.61"
                "085jjsls330yj1fnwykfzmb2f10zp6l7w4fhq81ng81574ghhpi3"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-ical-0.11.0
  (crate-source "ical" "0.11.0"
                "1xkrs9a48qzbzf6mbrnsvj9i51h2z44l7h7236d75dx88dssnz4v"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-indexmap-2.11.0
  (crate-source "indexmap" "2.11.0"
                "1sb3nmhisf9pdwfcxzqlvx97xifcvlh5g0rqj9j7i7qg8f01jj7j"))

(define rust-indicatif-0.18.0
  (crate-source "indicatif" "0.18.0"
                "1kg1wi3x9x15f22q99spfzcg5fzlmhcc5i6aqjxyssyh8vcld9kh"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-inotify-0.9.6
  (crate-source "inotify" "0.9.6"
                "1zxb04c4qccp8wnr3v04l503qpxzxzzzph61amlqbsslq4z9s1pq"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-instability-0.3.7
  (crate-source "instability" "0.3.7"
                "07f7k0cs1l8cdwxm46vy457bk880hgg6p83nfi777yqwv7bgxy8b"))

(define rust-interprocess-2.2.2
  (crate-source "interprocess" "2.2.2"
                "1sv2hf9ylxyn77sr1p92sl479ca6mjw1g03pdxmv6z413m4lhhc9"))

(define rust-intl-memoizer-0.5.3
  (crate-source "intl-memoizer" "0.5.3"
                "0gqn5wwhzacvj0z25r5r3l2pajg9c8i1ivh7g8g8dszm8pis439i"))

(define rust-intl-pluralrules-7.0.2
  (crate-source "intl_pluralrules" "7.0.2"
                "0wprd3h6h8nfj62d8xk71h178q7zfn3srxm787w4sawsqavsg3h7"))

(define rust-inventory-0.3.15
  (crate-source "inventory" "0.3.15"
                "0rspmi9qxz9hkajg4dx5hhwmcd3n3qw107hl3050hrs1izbd6n7r"))

(define rust-io-uring-0.7.8
  (crate-source "io-uring" "0.7.8"
                "04whnj5a4pml44jhsmmf4p87bpgr7swkcijx4yjcng8900pj0vmq"))

(define rust-ipnet-2.10.1
  (crate-source "ipnet" "2.10.1"
                "025p9wm94q1w2l13hbbr4cbmfygly3a2ag8g5s618l2jhq4l3hnx"))

(define rust-is-ci-1.2.0
  (crate-source "is_ci" "1.2.0"
                "0ifwvxmrsj4r29agfzr71bjq6y1bihkx38fbzafq5vl0jn1wjmbn"))

(define rust-is-debug-1.1.0
  (crate-source "is_debug" "1.1.0"
                "01yl28nv69wsqiyyhfbgx52yskpjyw5z4xq137c33ja3wb96dqhz"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-executable-1.0.4
  (crate-source "is_executable" "1.0.4"
                "1qlafm7f0zq0kzvbd4fhcfci4g9gxp6g3yqxjqsjj1zrssxbb8fl"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.32
  (crate-source "jobserver" "0.1.32"
                "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))

(define rust-js-sys-0.3.78
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.78"
                "0f17vdkpbarm0qlbqb0p1fyiy4l9bs62zvw3fv0ywb29g0shc2qc"))

(define rust-jsonpath-lib-polars-vendor-0.0.1
  (crate-source "jsonpath_lib_polars_vendor" "0.0.1"
                "0z8b92zhm1mrmb6fv95v2g5kqs6vmg5fl4zp3x3zf8knjia97ggl"))

(define rust-kqueue-1.0.8
  (crate-source "kqueue" "1.0.8"
                "033x2knkbv8d3jy6i9r32jcgsq6zm3g97zh5la43amkv3g5g2ivl"))

(define rust-kqueue-sys-1.0.4
  ;; TODO: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lean-string-0.5.0
  (crate-source "lean_string" "0.5.0"
                "1bjqpq869sg5cj6zdsxmcmbf634p38ykqvaalikvbgq0pnc9kr3m"))

(define rust-libc-0.2.174
  (crate-source "libc" "0.2.174"
                "0xl7pqvw7g2874dy3kjady2fjr4rhj5lxsnxkkhr5689jcr6jw8i"))

(define rust-libflate-1.4.0
  (crate-source "libflate" "1.4.0"
                "063xw2z477h3vh7j32y0f54a6nbndd7yf7rr5wpsvfw5nrqsxx2z"))

(define rust-libflate-lz77-1.2.0
  (crate-source "libflate_lz77" "1.2.0"
                "1gxc75fb2sk0xgrh3qxvxcx1l93yhmyxn9241r251wl5zj5klbd5"))

(define rust-libgit2-sys-0.18.0+1.9.0
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.0+1.9.0"
                "1v7zcw1kky338grxs70y7fwpy70g846bpa5yzvl9f5bybr31g8g1"))

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libproc-0.14.10
  (crate-source "libproc" "0.14.10"
                "1yzbhywjzvhi4353pni6k8jbpildd1qp66d1banvrbg5dfshk2p7"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libsqlite3-sys-0.35.0
  ;; TODO: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.35.0"
                "0gy1m6j1l94fxsirzp4h4rkrksf78rz7jy3px57qd1rcd8m1hg0k"))

(define rust-libssh2-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "libssh2-sys" "0.3.0"
                "1vkidqw5ll71ynqc93hgcq62iqkklzb5268zffd13ql7nwqa1j1d"))

(define rust-libz-rs-sys-0.5.1
  ;; TODO: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.1"
                "08a2grn3bp05696pd27s6kmq1icnbzffizl0nihic8m26y2phahp"))

(define rust-libz-sys-1.1.20
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.20"
                "0wp4i6zl385ilmcqafv61jwsk1mpk6yb8gpws9nwza00x19n9lfj"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-lipsum-0.9.1
  (crate-source "lipsum" "0.9.1"
                "0r40mf2cwh4fp9pdfcc1n8hjxw05w7galjvb1z23r5pq38jn0s33"))

(define rust-litemap-0.7.4
  (crate-source "litemap" "0.7.4"
                "012ili3vppd4952sh6y3qwcd0jkd0bq2qpr9h7cppc8sj11k7saf"))

(define rust-litrs-0.4.1
  (crate-source "litrs" "0.4.1"
                "19cssch9gc0x2snd9089nvwzz79zx6nzsi3icffpx25p4hck1kml"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-lockfree-object-pool-0.1.6
  (crate-source "lockfree-object-pool" "0.1.6"
                "0bjm2g1g1avab86r02jb65iyd7hdi35khn1y81z4nba0511fyx4k"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-lru-0.12.5
  (crate-source "lru" "0.12.5"
                "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))

(define rust-lscolors-0.20.0
  (crate-source "lscolors" "0.20.0"
                "1ax499r3kb1yhvmzs05x8f29slwnlxg5xm9hwdc9m84bvsjks631"))

(define rust-lsp-server-0.7.8
  (crate-source "lsp-server" "0.7.8"
                "1yanavncgsx0i0rj65q12ddfcwpvzx5x8wgiq4g9fzz1fgfc8qll"))

(define rust-lsp-textdocument-0.4.2
  (crate-source "lsp-textdocument" "0.4.2"
                "0b7dxa7x3v5z58nj5rsscv1kpifasbdp5x0v0wycsgafbxclsmid"))

(define rust-lsp-types-0.97.0
  (crate-source "lsp-types" "0.97.0"
                "0wb0yr2cdhlndjkcfyabr17ib0nvqa4v3zl5qm3aq13wl583adak"))

(define rust-lz4-1.28.0
  (crate-source "lz4" "1.28.0"
                "09b70ciyfbynzpy3yf501ab9f8chyyl0dppfh0cxv7d7njrfn7sd"))

(define rust-lz4-sys-1.11.1+lz4-1.10.0
  ;; TODO: Check bundled sources.
  (crate-source "lz4-sys" "1.11.1+lz4-1.10.0"
                "1rhqnhwq05fmlc2q39ipsq0vpi0xf6w6p22j6q5x637dqvbc1n3b"))

(define rust-mac-0.1.1
  (crate-source "mac" "0.1.1"
                "194vc7vrshqff72rl56f9xgb0cazyl4jda7qsv31m5l6xx7hq7n4"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-markup5ever-0.12.1
  (crate-source "markup5ever" "0.12.1"
                "0idy4vjihg2dw73j2vkb5kdghvga3bwnw0qx8jwci4m6xfxkmkhn"))

(define rust-markup5ever-0.35.0
  (crate-source "markup5ever" "0.35.0"
                "1hy1xh07jkm13j7vdnsphynl3z7hfmh99csjjvqzhl26jfffc7ri"))

(define rust-markup5ever-rcdom-0.3.0
  (crate-source "markup5ever_rcdom" "0.3.0"
                "065yb6zn9sfn7kqk5wwc48czsls5z3hzgrddk58fxgq16ymj3apd"))

(define rust-match-token-0.35.0
  (crate-source "match_token" "0.35.0"
                "1ksqd8li4kdd463cb2qf10d7d4j1m416y62xbzf47k0g6qzzv15c"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memmap2-0.9.5
  (crate-source "memmap2" "0.9.5"
                "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx"))

(define rust-miette-7.6.0
  (crate-source "miette" "7.6.0"
                "1dwjnnpcff4jzpf5ns1m19di2p0n5j31zmjv5dskrih7i3nfz62z"))

(define rust-miette-derive-7.6.0
  (crate-source "miette-derive" "7.6.0"
                "12w13a67n2cc37nzidvv0v0vrvf4rsflzxz6slhbn3cm9rqjjnyv"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.5
  (crate-source "mime_guess" "2.0.5"
                "03jmg3yx6j39mg0kayf7w4a886dl3j15y8zs119zw01ccy74zi7p"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-mockito-1.7.0
  (crate-source "mockito" "1.7.0"
                "0j5acfmm1ki098rwn63g0swn2f69ljf00x03givybdyr33jf0q3p"))

(define rust-moka-0.12.10
  (crate-source "moka" "0.12.10"
                "09h9ww66vxrkizma99n7ib2fm91crkw4msp650j2i94lr911ccm9"))

(define rust-multipart-rs-0.1.13
  (crate-source "multipart-rs" "0.1.13"
                "1wj5jgbrq7svcqdahxi17j8vws8nsz6a5y9f6ir51ajjgq7f1jk4"))

(define rust-native-tls-0.2.12
  (crate-source "native-tls" "0.2.12"
                "0rkl65z70n7sy4d5w0qa99klg1hr43wx6kcprk4d2n9xr2r4wqd8"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-nix-0.28.0
  (crate-source "nix" "0.28.0"
                "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-notify-6.1.1
  (crate-source "notify" "6.1.1"
                "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))

(define rust-notify-debouncer-full-0.3.2
  (crate-source "notify-debouncer-full" "0.3.2"
                "0kpkz1y4v2kwh5rgkhx4h2fxnwqka30lsrcy2vzwk2cpfdkd2zzv"))

(define rust-now-0.1.3
  (crate-source "now" "0.1.3"
                "1l135786rb43rjfhwfdj7hi3b5zxxyl9gwf15yjz18cp8f3yk2bd"))

(define rust-ntapi-0.4.1
  (crate-source "ntapi" "0.4.1"
                "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-nucleo-matcher-0.3.1
  (crate-source "nucleo-matcher" "0.3.1"
                "11dc5kfin1n561qdcg0x9aflvw876a8vldmqjhs5l6ixfcwgacxz"))

(define rust-num-0.4.3
  (crate-source "num" "0.4.3"
                "08yb2fc1psig7pkzaplm495yp7c30m4pykpkwmi5bxrgid705g9m"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-format-0.4.4
  (crate-source "num-format" "0.4.4"
                "1hvjmib117jspyixfr76f900mhz5zfn71dnyqg9iywb339vxjlm6"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-threads-0.1.7
  (crate-source "num_threads" "0.1.7"
                "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-number-prefix-0.4.0
  (crate-source "number_prefix" "0.4.0"
                "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))

(define rust-objc-sys-0.3.5
  ;; TODO: Check bundled sources.
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-foundation-0.3.1
  (crate-source "objc2-core-foundation" "0.3.1"
                "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-encode-4.0.3
  (crate-source "objc2-encode" "4.0.3"
                "1y7hjg4k828zhn4fjnbidrz3vzw4llk9ldy92drj47ydjc9yg4bq"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-io-kit-0.3.1
  (crate-source "objc2-io-kit" "0.3.1"
                "02iwv7pppxvna72xwd7y5q67hrnbn5v73xikc3c1rr90c56wdhbi"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-object-0.36.5
  (crate-source "object" "0.36.5"
                "0gk8lhbs229c68lapq6w6qmnm4jkj48hrcw5ilfyswy514nhmpxf"))

(define rust-object-store-0.12.1
  (crate-source "object_store" "0.12.1"
                "05psylz0ap3lbnxbhnm3gr2vfnl3sa9wi23369swy31w8dmw2jnr"))

(define rust-oem-cp-2.1.0
  (crate-source "oem_cp" "2.1.0"
                "02iln0d65vf92svha9av4p0p9v6p48i613f35saq8xpc2yn2i8kx"))

(define rust-omnipath-0.1.6
  (crate-source "omnipath" "0.1.6"
                "0xd5a4xwsfmhzk59v6wz65f59rk16d7gvkg90w1qhb0jg08b7bc0"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-open-5.3.1
  (crate-source "open" "5.3.1"
                "177npnl443gln9ypzcpsj5fjw0yyslg2bai093740p6ip3q55k9y"))

(define rust-openssl-0.10.72
  (crate-source "openssl" "0.10.72"
                "1np54pm6hw512rmfjv3kc54h8yvf51mdlm8a8cc33xx1b1yympzy"))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-openssl-src-300.4.1+3.4.0
  ;; TODO: Check bundled sources.
  (crate-source "openssl-src" "300.4.1+3.4.0"
                "1337svym5imvq9ww04xh0ss38krhbhfb7l92ar5l2qlc2g2fm97s"))

(define rust-openssl-sys-0.9.107
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.107"
                "01yydv8yaagdnapvair8b6rggf225lwb854h99s9qx44rnd9g242"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-ordered-multimap-0.7.3
  (crate-source "ordered-multimap" "0.7.3"
                "0ygg08g2h381r3zbclba4zx4amm25zd2hsqqmlxljc00mvf3q829"))

(define rust-os-display-0.1.3
  (crate-source "os_display" "0.1.3"
                "0xfgfqvfg5nyidv5p85fb87l0mif1nniisxarw6npd4jv2x2jqks"))

(define rust-os-pipe-1.2.1
  (crate-source "os_pipe" "1.2.1"
                "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz"))

(define rust-outref-0.5.1
  (crate-source "outref" "0.5.1"
                "0ynw7nb89603gkfi83f9chsf76ds3b710gxfn12yyawrzl7pcc20"))

(define rust-owo-colors-4.1.0
  (crate-source "owo-colors" "4.1.0"
                "0mms4sbisxm1w8v08qz85m90sv861xg4ahil85587kb9cmzpcdzv"))

(define rust-papergrid-0.17.0
  (crate-source "papergrid" "0.17.0"
                "1wg3k8kgv0rgxqszf5c6dv6347mm58qb5kii0q4g9n2iif614y39"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parse-datetime-0.11.0
  (crate-source "parse_datetime" "0.11.0"
                "1sdkn97dzvh27v5gdfjh84idpm1zby748m1sszphqiks4lkpvdy5"))

(define rust-parse-zoneinfo-0.3.1
  (crate-source "parse-zoneinfo" "0.3.1"
                "093cs8slbd6kyfi6h12isz0mnaayf5ha8szri1xrbqj4inqhaahz"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-peresil-0.3.0
  (crate-source "peresil" "0.3.0"
                "0mwyw03yqp0yqdjf4a89vn86szxaksmxvgzv1j2nw69fsmp8hn7n"))

(define rust-pest-2.7.15
  (crate-source "pest" "2.7.15"
                "1p4rq45xprw9cx0pb8mmbfa0ih49l0baablv3cpfdy3c1pkayz4b"))

(define rust-pest-consume-1.1.3
  (crate-source "pest_consume" "1.1.3"
                "0sskbz2hlqdvrjrp0nxww5diaggsccp2ziql5qaff62xs4178i3r"))

(define rust-pest-consume-macros-1.1.0
  (crate-source "pest_consume_macros" "1.1.0"
                "0a9zg5zishafz0hhmp2byfd04h22naka0sy1q5739jwrm2kk11lx"))

(define rust-pest-derive-2.7.15
  (crate-source "pest_derive" "2.7.15"
                "0zpmcd1jv1c53agad5b3jb66ylxlzyv43x1bssh8fs7w3i11hrc1"))

(define rust-pest-generator-2.7.15
  (crate-source "pest_generator" "2.7.15"
                "0yrpk5ymc56pffv7gqr5rkv92p3dc6s73lb8hy1wf3w77byrc4vx"))

(define rust-pest-meta-2.7.15
  (crate-source "pest_meta" "2.7.15"
                "1skx7gm932bp77if63f7d72jrk5gygj39d8zsfzigmr5xa4q1rg1"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"))

(define rust-phf-0.11.2
  (crate-source "phf" "0.11.2"
                "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))

(define rust-phf-codegen-0.11.2
  (crate-source "phf_codegen" "0.11.2"
                "0nia6h4qfwaypvfch3pnq1nd2qj64dif4a6kai3b7rjrsf49dlz8"))

(define rust-phf-generator-0.11.2
  (crate-source "phf_generator" "0.11.2"
                "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))

(define rust-phf-macros-0.11.2
  (crate-source "phf_macros" "0.11.2"
                "0js61lc0bhzzrbd9vhpcqp11vvwckdkz3g7k95z5h1k651p68i1l"))

(define rust-phf-shared-0.11.2
  (crate-source "phf_shared" "0.11.2"
                "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))

(define rust-pin-project-lite-0.2.15
  (crate-source "pin-project-lite" "0.2.15"
                "1zz4xif3iknfrpmvqmh0pcc9mx4cxm28jywqydir3pimcla1wnli"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-planus-1.1.1
  (crate-source "planus" "1.1.1"
                "0wdgc8py2c7c77x4m72q37m7csxpkzigcf08d4fvwaki9cyqxbrx"))

(define rust-platform-info-2.0.4
  (crate-source "platform-info" "2.0.4"
                "0flc79bbc4vr1yb1jx8ahs04khxayvbxgz7fg46xfn6h0pypy1wi"))

(define rust-plist-1.8.0
  (crate-source "plist" "1.8.0"
                "01qyv51ljbvhjbg8mva5c802b3dzrr95y6nd23wjh52xbjhvw3kl"))

(define rust-polars-0.51.0
  (crate-source "polars" "0.51.0"
                "15xn470zsxrp3ki5zhwkrhyl6xwdsyrah8pz3mllx5bbsnszxxx5"))

(define rust-polars-arrow-0.51.0
  (crate-source "polars-arrow" "0.51.0"
                "0j018k7pd7jsql63b0jpknny3hs0albfwb6vlgpb6q9r6k9gxd1j"))

(define rust-polars-arrow-format-0.2.0
  (crate-source "polars-arrow-format" "0.2.0"
                "0y2f8gaizsnzabx858j8h2jygxs91pi56w6vsz75w05y2k2h8g46"))

(define rust-polars-compute-0.51.0
  (crate-source "polars-compute" "0.51.0"
                "0gs1simdn1cfwpni6589x4minrsss780k7r14nh90jjfvaz8b1qk"))

(define rust-polars-core-0.51.0
  (crate-source "polars-core" "0.51.0"
                "07ch31awmi14bfljfs2zj3whv5dycia3dlyhn4mh7fvdxw41yyz7"))

(define rust-polars-dtype-0.51.0
  (crate-source "polars-dtype" "0.51.0"
                "0n3ikd5p7vk7qg3s0ccwqyljk2zdg520dn648r2vws3ill73vi49"))

(define rust-polars-error-0.51.0
  (crate-source "polars-error" "0.51.0"
                "1h33an4nza4ar5gxkkmgf0hhv7xd802894g37rkr92wzync5vjxr"))

(define rust-polars-expr-0.51.0
  (crate-source "polars-expr" "0.51.0"
                "0p4nn5dcy1m6ijxb2xrl5jsq6xn232y1v8av2d4n64yg32w32f9l"))

(define rust-polars-io-0.51.0
  (crate-source "polars-io" "0.51.0"
                "1r074g5jy60vnm57d3lqpj9w7zglqv8q3a19h9424l8mp1j8qf0h"))

(define rust-polars-json-0.51.0
  (crate-source "polars-json" "0.51.0"
                "075l7gnwij2jarn4lbxcqpx0n82gb3qzv55siqy1i0ia5421lvdj"))

(define rust-polars-lazy-0.51.0
  (crate-source "polars-lazy" "0.51.0"
                "0iw180ylwgdfbfcpff38qbkq234nav7hc76zc33a0kpsqb3f5dhg"))

(define rust-polars-mem-engine-0.51.0
  (crate-source "polars-mem-engine" "0.51.0"
                "0w9p28fmzm412h7hkjii8z2b4p57wpksacl1ip18fd95ivlmda10"))

(define rust-polars-ops-0.51.0
  (crate-source "polars-ops" "0.51.0"
                "0xmxpb22nwgdhjcxrslsila3lr3ndsvbwj2mfw2vmjgxfchhdxmc"))

(define rust-polars-parquet-0.51.0
  (crate-source "polars-parquet" "0.51.0"
                "0s86wsda9wfqvp2fq65j6bz2rgv4nfcr5f641pgp1h6yh28pc7fc"))

(define rust-polars-parquet-format-0.1.0
  (crate-source "polars-parquet-format" "0.1.0"
                "1qcw67m8mzc7xndvkmc0n5d5c2zipsrjxy6rjizcbnz8rwyj89f0"))

(define rust-polars-plan-0.51.0
  (crate-source "polars-plan" "0.51.0"
                "186hwqw3dh038626pbgkrk8qj22gd2jjnbdb0zj4yj747bis5lqw"))

(define rust-polars-row-0.51.0
  (crate-source "polars-row" "0.51.0"
                "1h5bxl2ypz4xjsn1q12bq0bi2s2cfkpg6rdfyd6p4j73w0blywqq"))

(define rust-polars-schema-0.51.0
  (crate-source "polars-schema" "0.51.0"
                "1qw59acwvmxyv8c123wakgnv50h4xb8lx1d9c5v1dm847sqilv4f"))

(define rust-polars-sql-0.51.0
  (crate-source "polars-sql" "0.51.0"
                "048apjdvs3s84p1jfdnz6d4ws33sibld6124k5jd9h9cl1npdry4"))

(define rust-polars-stream-0.51.0
  (crate-source "polars-stream" "0.51.0"
                "050za8hfsx85skpwkypvcz6l9v7m6d7pw5jd8bm9s7x03v5cdxii"))

(define rust-polars-time-0.51.0
  (crate-source "polars-time" "0.51.0"
                "1vkdsyy119xzjr1i1hly2b3901c8kq7nc59pp2h89ad7g7iad8zn"))

(define rust-polars-utils-0.51.0
  (crate-source "polars-utils" "0.51.0"
                "0a9zjyg1kc0q78vpjmsqdi3aiydra6gdf3y7zgbj4m0f3c16gcjp"))

(define rust-pori-0.0.0
  (crate-source "pori" "0.0.0"
                "01p9g4fn3kasnmwj8i4plzk6nnnk7ak2qsfcv9b9y4zcilrkv9m4"))

(define rust-portable-atomic-1.10.0
  (crate-source "portable-atomic" "1.10.0"
                "1rjfim62djiakf5rcq3r526hac0d1dd9hwa1jmiin7q7ad2c4398"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.20
  (crate-source "ppv-lite86" "0.2.20"
                "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))

(define rust-precomputed-hash-0.1.1
  (crate-source "precomputed-hash" "0.1.1"
                "075k9bfy39jhs53cb2fpb9klfakx2glxnf28zdw08ws6lgpq6lwj"))

(define rust-predicates-3.1.2
  (crate-source "predicates" "3.1.2"
                "15rcyjax4ykflw5425wsyzcfkgl08c9zsa8sdlsrmhj0fv68d43y"))

(define rust-predicates-core-1.0.8
  (crate-source "predicates-core" "1.0.8"
                "0c8rl6d7qkcl773fw539h61fhlgdg7v9yswwb536hpg7x2z7g0df"))

(define rust-predicates-tree-1.0.11
  (crate-source "predicates-tree" "1.0.11"
                "04zv0i9pjfrldnvyxf4y07n243nvk3n4g03w2k6nccgdjp8l1ds1"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-print-positions-0.6.1
  (crate-source "print-positions" "0.6.1"
                "026jzdf63b37bb9ix3mpczln2pqylsiwkkxhikj05x9y1r3r7x8x"))

(define rust-proc-macro-error-attr2-2.0.0
  (crate-source "proc-macro-error-attr2" "2.0.0"
                "1ifzi763l7swl258d8ar4wbpxj4c9c2im7zy89avm6xv6vgl5pln"))

(define rust-proc-macro-error2-2.0.1
  (crate-source "proc-macro-error2" "2.0.1"
                "00lq21vgh7mvyx51nwxwf822w2fpww1x0z8z0q47p8705g2hbv0i"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-procfs-0.17.0
  (crate-source "procfs" "0.17.0"
                "17swyjqinpb745f07dpdi7c8q37hxvhx9xmmsi2dhxaj2kc74nyc"))

(define rust-procfs-core-0.17.0
  (crate-source "procfs-core" "0.17.0"
                "1v0jdbyc1rq1x22m0wn7n4iq4h86gdls38wqfg06zc29hcnz1793"))

(define rust-psm-0.1.24
  (crate-source "psm" "0.1.24"
                "0428cnwx8i9dhkcjwzap0amg9cjk8nhj0xr5hkhm6zl543r9y2r0"))

(define rust-pure-rust-locales-0.8.1
  (crate-source "pure-rust-locales" "0.8.1"
                "0fkkwggiq2053rmiah2h06dz6w3yhy9pa82g30vy3sbcmqcgv40i"))

(define rust-pwd-1.4.0
  (crate-source "pwd" "1.4.0"
                "18p4j95sqqcxn3fbm6gbi7klxp8n40xmcjqy9vz1ww5rg461rivj"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-xml-0.36.2
  (crate-source "quick-xml" "0.36.2"
                "1zj3sjcjk6sn544wb2wvhr1km5f9cy664vzclygfsnph9mxrlr7p"))

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quick-xml-0.38.3
  (crate-source "quick-xml" "0.38.3"
                "12bvsbnnmlnq9xg9in3h3080ag3sisafgpcn7lqyzhkz93kk58j2"))

(define rust-quickcheck-1.0.3
  (crate-source "quickcheck" "1.0.3"
                "1mjhkfqwrb8mdyxdqr4zzbj1rm5dfx25n9zcc25lb6fxwiw673sq"))

(define rust-quickcheck-macros-1.1.0
  (crate-source "quickcheck_macros" "1.1.0"
                "0jn17bziphar3kmn2kw445a2vba1p3wycarnsf49ligq8a5y67pp"))

(define rust-quinn-0.11.6
  (crate-source "quinn" "0.11.6"
                "1vq55p4kfc4zjxj58xrpf3kcjjqi4mn0wf52a5rzkiky4w46isb2"))

(define rust-quinn-proto-0.11.9
  (crate-source "quinn-proto" "0.11.9"
                "0p8k3iqd0rcxc7b6m2yyijhw4bpfwa61lyzigwvjwzax97rmxzm2"))

(define rust-quinn-udp-0.5.8
  (crate-source "quinn-udp" "0.5.8"
                "09xmwrxikc69ilsmghxls3s0gq295hli366k1na2ggv8zwg4pkaj"))

(define rust-quote-1.0.37
  (crate-source "quote" "1.0.37"
                "1brklraw2g34bxy9y4q1nbrccn7bv36ylihv12c9vlcii55x7fdm"))

(define rust-quoted-printable-0.5.1
  (crate-source "quoted_printable" "0.5.1"
                "0wvwq6w6rdsx1yxzr7ckspff0qk0q9252dzmxrd4c0kv97c9n334"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.0
  (crate-source "rand" "0.9.0"
                "156dyvsfa6fjnv6nx5vzczay1scy5183dvjchd7bvs47xd5bjy9p"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-distr-0.5.1
  (crate-source "rand_distr" "0.5.1"
                "0qvlzxq4a2rvrf3wq0xq1bfw8iy9zqm6jlmbywqzld6g1paib1ka"))

(define rust-ratatui-0.29.0
  (crate-source "ratatui" "0.29.0"
                "0yqiccg1wmqqxpb2sz3q2v3nifmhsrfdsjgwhc2w40bqyg199gga"))

(define rust-raw-cpuid-11.2.0
  (crate-source "raw-cpuid" "11.2.0"
                "1c77cmsn7rj6knwwrg2y9nl46wss5p9jq3wzxvr1a5k6bhql1chs"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-recursive-0.1.1
  (crate-source "recursive" "0.1.1"
                "0gmlaih5kyqc1pkbk0klqr9m65c4bvz6j0mwn68z8q5pxcys91h7"))

(define rust-recursive-proc-macro-impl-0.1.1
  (crate-source "recursive-proc-macro-impl" "0.1.1"
                "12z3wy2wa4l2dpfdb5vhaaiy78l130x5w9fflb0py1ql0sz9y03n"))

(define rust-recvmsg-1.0.0
  (crate-source "recvmsg" "1.0.0"
                "0xa173gbg1cx8q7wyzi6c4kmcsz5rka68r4jb6kg14icskax9vfk"))

(define rust-redox-syscall-0.5.8
  (crate-source "redox_syscall" "0.5.8"
                "0d48ylyd6gsamynyp257p6n2zl4dw2fhnn5z9y3nhgpri6rn5a03"))

(define rust-redox-users-0.5.2
  (crate-source "redox_users" "0.5.2"
                "1b17q7gf7w8b1vvl53bxna24xl983yn7bd00gfbii74bcg30irm4"))

(define rust-reedline-0.43.0
  (crate-source "reedline" "0.43.0"
                "1w51379xksz5m5ms3szr2dirpkvmsyv2fh5g4abg8sai4bszkz5h"))

(define rust-ref-cast-1.0.23
  (crate-source "ref-cast" "1.0.23"
                "0ca9fi5jsdibaidi2a55y9i1k1q0hvn4f6xlm0fmh7az9pwadw6c"))

(define rust-ref-cast-impl-1.0.23
  (crate-source "ref-cast-impl" "1.0.23"
                "1rpkjlsr99g8nb5ripffz9n9rb3g32dmw83x724l8wykjgkh7hxw"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-lite-0.1.6
  (crate-source "regex-lite" "0.1.6"
                "0almvx3z75f611pdcd9mslh7zxg76zh3shifql4ndch6mn3rb92k"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-relative-path-1.9.3
  (crate-source "relative-path" "1.9.3"
                "1limlh8fzwi21g0473fqzd6fln9iqkwvzp3816bxi31pkilz6fds"))

(define rust-reqwest-0.12.9
  (crate-source "reqwest" "0.12.9"
                "0vq40h75fmrkfjyyjxl84g0pzjzz0n989ag1cajy17g78spn4z57"))

(define rust-rfc2047-decoder-1.0.6
  (crate-source "rfc2047-decoder" "1.0.6"
                "0afyg0k2hagnirvv5rk2ysr3khz8ab5ifdap3dsnli9121fm8dmw"))

(define rust-ring-0.17.13
  (crate-source "ring" "0.17.13"
                "1vjhhlmpqqd9lc53ffjj1yk203188n2km27g3myvssm15a1mvb3h"))

(define rust-rle-decode-fast-1.0.3
  (crate-source "rle-decode-fast" "1.0.3"
                "08kljzl29rpm12fiz0qj5pask49aiswdvcjigdcq73s224rgd0im"))

(define rust-rmcp-0.8.1
  (crate-source "rmcp" "0.8.1"
                "074w8vzvbwqm141wix1d0yh8xhxlsp3f7jlnikysbz49izdaqdbg"))

(define rust-rmcp-macros-0.8.1
  (crate-source "rmcp-macros" "0.8.1"
                "0x52aklb7g6nd5nbnjslxb6b60s4amx8zq827nfbf8xa18idbwf9"))

(define rust-rmp-0.8.14
  (crate-source "rmp" "0.8.14"
                "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))

(define rust-rmp-serde-1.3.0
  (crate-source "rmp-serde" "1.3.0"
                "1nylmh7w2vpa1bwrnx1jfp2l4yz6i5qrmpic5zll166gfyj9kraj"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rstest-0.23.0
  (crate-source "rstest" "0.23.0"
                "0d90hr3i2yajzgpzvsh6p2yjzmcb3nm8884xdbb5sswvwmdmhb0a"))

(define rust-rstest-macros-0.23.0
  (crate-source "rstest_macros" "0.23.0"
                "0nmdm7a4ysihnh0zz6w6gqrmw205zfp7xqkb2id3858vg20afpl2"))

(define rust-rstest-reuse-0.7.0
  (crate-source "rstest_reuse" "0.7.0"
                "057y4v1rh9br58n2m3xqvm8xyx8k96jpgibgls3sah78f93gpa5k"))

(define rust-rusqlite-0.37.0
  (crate-source "rusqlite" "0.37.0"
                "0gqzwykyfaaddq5rg1jk0940wby6ifarnwp3fcakbq90ggjscp0n"))

(define rust-rust-decimal-1.36.0
  (crate-source "rust_decimal" "1.36.0"
                "0mgmplkpawx9kggc4v3qymmdxx71dx1qsf1lsqp2pi9w7q7di0mh"))

(define rust-rust-embed-8.7.0
  (crate-source "rust-embed" "8.7.0"
                "17f4pribh9nd97szi8zzc2a5xd5myxfjwi5vrvvrmfgwa3pc1yz5"))

(define rust-rust-embed-impl-8.7.0
  (crate-source "rust-embed-impl" "8.7.0"
                "0bkh66kzmqv1i478d24nsv4nf89crhs732lblcy6dxp3lb4iix3b"))

(define rust-rust-embed-utils-8.7.0
  (crate-source "rust-embed-utils" "8.7.0"
                "08cfp8x1nw1p272128hfwr9fvnlbg7dmafbbs1ji5q3z2jampm88"))

(define rust-rust-ini-0.21.1
  (crate-source "rust-ini" "0.21.1"
                "17s0fmfba05i9cnfwfy02wp7mf5wkapis5x1d68vgvmnw7q0wcaf"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.0
  (crate-source "rustc-hash" "2.1.0"
                "15yln6fmqlbg0k35r748h8g9xsd637ri23xihq81jb03ncwq1yy7"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.42
  (crate-source "rustix" "0.38.42"
                "11fvprv3p450ggyqacp7sdpjbbsgm5zvfjwnzy8bfbmbrf7c6ggr"))

(define rust-rustix-1.0.7
  (crate-source "rustix" "1.0.7"
                "0rhjh16bnxi86nrn9qwcnw5632mvd5m1vdy61s4n9zz7mzb867n7"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.23.28
  (crate-source "rustls" "0.23.28"
                "0hv6sk3r60vw11in2p8phpjd132684b4wg3zac456lzl1ghy6q3i"))

(define rust-rustls-native-certs-0.6.3
  (crate-source "rustls-native-certs" "0.6.3"
                "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))

(define rust-rustls-native-certs-0.8.1
  (crate-source "rustls-native-certs" "0.8.1"
                "1ls7laa3748mkn23fmi3g4mlwk131lx6chq2lyc8v2mmabfz5kvz"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))

(define rust-rustls-pki-types-1.12.0
  (crate-source "rustls-pki-types" "1.12.0"
                "0yawbdpix8jif6s8zj1p2hbyb7y3bj66fhx0y7hyf4qh4964m6i2"))

(define rust-rustls-platform-verifier-0.5.3
  (crate-source "rustls-platform-verifier" "0.5.3"
                "1hdc6h6p89x7bz3jsxj9xj7wv1swg4qqpp238i0cb3j0fvd7qy0r"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.103.3
  (crate-source "rustls-webpki" "0.103.3"
                "0ddl9qxx94iyichk05r7l30d9dxfd35ybffhsxpsr9pppki2z9z4"))

(define rust-rustversion-1.0.18
  (crate-source "rustversion" "1.0.18"
                "0j2207vmgrcxwwwvknfn3lwv4i8djhjnxlvwdnz8bwijqqmrz08f"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-scc-2.4.0
  (crate-source "scc" "2.4.0"
                "1k2nwz3bysf1s3r5g437vq9xfm9i4sadfzn5c0k8xx7ynx3g1rj6"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))

(define rust-schemars-1.0.4
  (crate-source "schemars" "1.0.4"
                "1l7w773jfk6mz0v8wpahp60aslksjijlbm65ysi4y5mwj520rll2"))

(define rust-schemars-derive-1.0.4
  (crate-source "schemars_derive" "1.0.4"
                "107sprdfa5kacifxq41qv5ccv7a78msxyr8ikz0qs4qxdlwj1l1k"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-scraper-0.24.0
  (crate-source "scraper" "0.24.0"
                "1naakil17f51inhrkjjysqarajld2ras90cjz559ay3fj56s5wz5"))

(define rust-scroll-0.11.0
  (crate-source "scroll" "0.11.0"
                "1nhrhpzf95pxbcjjy222blwf8rl3adws6vsqax0yzyxsa6snbi84"))

(define rust-scroll-derive-0.11.1
  (crate-source "scroll_derive" "0.11.1"
                "1bi5ljnzksvqhic6j7i2a2ap41s78xr0gifkgjxdxlj63pw4kc8x"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-sdd-3.0.10
  (crate-source "sdd" "3.0.10"
                "1jj1brjjasx7r3lf6iyhhrpglx47vzr0z1qi1n0fcszjzv5wy3a9"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-3.0.1
  (crate-source "security-framework" "3.0.1"
                "1j1rpyiwq698dyyq2lnnws8hzknw8r32dy9cx9jc7gljgrh5lhg1"))

(define rust-security-framework-sys-2.12.1
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.12.1"
                "18pafp0bn41bcbm66qrhb3pg4c8dddvc28jdr51mb2y57lqcffgs"))

(define rust-selectors-0.31.0
  (crate-source "selectors" "0.31.0"
                "1dhgxvq8p6iald0iwlq2r5169d31iy75vyzwglp7vkxz8fpbd1an"))

(define rust-self-cell-1.2.0
  (crate-source "self_cell" "1.2.0"
                "0jg70srf4hzrw96x8iclgf6i8dfgm1x8ds2i7yzcgq0i8njraz8g"))

(define rust-semver-1.0.23
  (crate-source "semver" "1.0.23"
                "12wqpxfflclbq4dv8sa6gchdh92ahhwn4ci1ls22wlby3h57wsb1"))

(define rust-serde-1.0.225
  (crate-source "serde" "1.0.225"
                "07dxpjh0g1mq3md9yvn7jbgssgcizcircf23f04xml1mwbg28v7x"))

(define rust-serde-core-1.0.225
  (crate-source "serde_core" "1.0.225"
                "10v3z58j5k6xhdxh90xgrv20wlnz5fnl67n04jdm47nbl3wmd4v5"))

(define rust-serde-derive-1.0.225
  (crate-source "serde_derive" "1.0.225"
                "05j5zj2jdba3jnm7kh3fpljmhngmsa8pp5x495lpc7wbyynkda8f"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-json-1.0.133
  (crate-source "serde_json" "1.0.133"
                "0xz3bswa527wln3fy0qb7y081nx3cp5yy1ggjhi6n5mrfcjfpz67"))

(define rust-serde-repr-0.1.19
  (crate-source "serde_repr" "0.1.19"
                "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-stacker-0.1.14
  (crate-source "serde_stacker" "0.1.14"
                "0jhgpgcki4gqa8z28g1bxliz6ilf9wsak4r2ybpyfjqcsmsn74yl"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-serial-test-3.2.0
  (crate-source "serial_test" "3.2.0"
                "1a8zg87gi28952hzj363ykwd8p1ssrakl1gi3f4xdqa4y84q298v"))

(define rust-serial-test-derive-3.2.0
  (crate-source "serial_test_derive" "3.2.0"
                "1vwyz2k5kiy5jmba0fvp6ph8ia707801bz918n2ff7bm11d2csax"))

(define rust-servo-arc-0.4.0
  (crate-source "servo_arc" "0.4.0"
                "06ljch4isnnbv1xpwhjajz4a4mpc7ki47ys9n9yn98kqjhjc8rdf"))

(define rust-sha1-smol-1.0.1
  (crate-source "sha1_smol" "1.0.1"
                "0pbh2xjfnzgblws3hims0ib5bphv7r5rfdpizyh51vnzvnribymv"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-shadow-rs-1.4.0
  (crate-source "shadow-rs" "1.4.0"
                "0cmfaz8lf2dqhq9igq4vg7dp6385qx4k641n53ibq9pnrs1q3lbj"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-json-0.15.1
  (crate-source "simd-json" "0.15.1"
                "04i1z4af0sh74hpydlfjdibn450x6g45whz5cq09jwa7nlkgcqn9"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-simplelog-0.12.2
  (crate-source "simplelog" "0.12.2"
                "1h59cp84gwdmbxiljq6qmqq1x3lv9ikc1gb32f5ya7pgzbdpl98n"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-skiplist-0.6.0
  (crate-source "skiplist" "0.6.0"
                "18cf3h46law556wfiglv2px45cscvhpkx58422aw4xri5llgsm7k"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-snap-1.1.1
  (crate-source "snap" "1.1.1"
                "0fxw80m831l76a5zxcwmz2aq7mcwc1pp345pnljl4cv1kbxnfsqv"))

(define rust-socket2-0.5.8
  (crate-source "socket2" "0.5.8"
                "1s7vjmb5gzp3iaqi94rh9r63k9cj00kjgbfn7gn60kmnk6fjcw69"))

(define rust-socks-0.3.4
  (crate-source "socks" "0.3.4"
                "12ymihhib0zybm6n4mrvh39hj1dm0ya8mqnqdly63079kayxphzh"))

(define rust-sqlparser-0.53.0
  (crate-source "sqlparser" "0.53.0"
                "1s2w4xwr79wp1c51nxrq9awk9rfc3x4ssma84srhj8ir9h8ji985"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stacker-0.1.17
  (crate-source "stacker" "0.1.17"
                "0yya93mkaxidcxcc1jhfvb58xpmnp4ikyyqsmyc5xnxbalyqi73r"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-streaming-decompression-0.1.2
  (crate-source "streaming-decompression" "0.1.2"
                "1wscqj3s30qknda778wf7z99mknk65p0h9hhs658l4pvkfqw6v5z"))

(define rust-streaming-iterator-0.1.9
  (crate-source "streaming-iterator" "0.1.9"
                "0845zdv8qb7zwqzglpqc0830i43xh3fb6vqms155wz85qfvk28ib"))

(define rust-strength-reduce-0.2.4
  (crate-source "strength_reduce" "0.2.4"
                "10jdq9dijjdkb20wg1dmwg447rnj37jbq0mwvbadvqi2gys5x2gy"))

(define rust-string-cache-0.8.9
  (crate-source "string_cache" "0.8.9"
                "03z7km2kzlwiv2r2qifq5riv4g8phazwng9wnvs3py3lzainnxxz"))

(define rust-string-cache-codegen-0.5.4
  (crate-source "string_cache_codegen" "0.5.4"
                "181ir4d6y053s1kka2idpjx5g9d9jgll6fy517jhzzpi2n3r44f7"))

(define rust-strip-ansi-escapes-0.2.1
  (crate-source "strip-ansi-escapes" "0.2.1"
                "0980min1s9f5g47rwlq8l9njks952a0jlz0v7yxrm5p7www813ra"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-strum-macros-0.27.2
  (crate-source "strum_macros" "0.27.2"
                "19xwikxma0yi70fxkcy1yxcv0ica8gf3jnh5gj936jza8lwcx5bn"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-supports-color-3.0.2
  (crate-source "supports-color" "3.0.2"
                "1mk7r2j6l7zmqk3pg7av0l6viq413lmk1vz4bjnf9lnq5liwfky6"))

(define rust-supports-hyperlinks-3.1.0
  (crate-source "supports-hyperlinks" "3.1.0"
                "12r8d8ckdx78rhdsavh08gg4210i3bmcn2prm7k2s5b37knl8kw0"))

(define rust-supports-unicode-3.0.0
  (crate-source "supports-unicode" "3.0.0"
                "1qpc344453x3ai4k9iygxnbk6lr2nw5jflj8ns5q3dbcmwq1lh5p"))

(define rust-sxd-document-0.3.2
  (crate-source "sxd-document" "0.3.2"
                "0y10shqmy9xb73g403rg1108wsagny9d8jrcm081pbwzpqvjzn4l"))

(define rust-sxd-xpath-0.4.2
  (crate-source "sxd-xpath" "0.4.2"
                "1sin3g8lzans065gjcwrpm7gdpwdpdg4rpi91rlvb1q8sfjrvqrn"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.90
  (crate-source "syn" "2.0.90"
                "0cfg5dsr1x0hl6b9hz08jp1197mx0rq3xydqmqaws36xlms3p7ci"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-sysinfo-0.36.1
  (crate-source "sysinfo" "0.36.1"
                "0z9141y32amzlg87ky0swsi4myhwngcdpfmjnzzvkrv0a1s00a15"))

(define rust-tabled-0.20.0
  (crate-source "tabled" "0.20.0"
                "0zawf8zg5frprmqjygldggn8zj4cyw0b7qbilw2hhdndzghjx6p3"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-tango-bench-0.6.0
  (crate-source "tango-bench" "0.6.0"
                "0gj2jgfdmwhrdggqh3yp8h33n1jrz6f3drmzg3nny83gihsj4y15"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tendril-0.4.3
  (crate-source "tendril" "0.4.3"
                "1c3vip59sqwxn148i714nmkrvjzbk7105vj0h92s6r64bw614jnj"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-terminal-size-0.4.1
  (crate-source "terminal_size" "0.4.1"
                "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk"))

(define rust-termtree-0.4.1
  (crate-source "termtree" "0.4.1"
                "0xkal5l2r3r9p9j90x35qy4npbdwxz4gskvbijs6msymaangas9k"))

(define rust-testing-table-0.3.0
  (crate-source "testing_table" "0.3.0"
                "1k0l036hgxmvjzr8ngc57ngkhnza3p9xh6cyc5jlz8lmk7iam38g"))

(define rust-textwrap-0.16.1
  (crate-source "textwrap" "0.16.1"
                "1fgqn3mg9gdbjxwfxl76fg0qiq53w3mk4hdh1x40jylnz39k9m13"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-time-0.3.37
  (crate-source "time" "0.3.37"
                "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm"))

(define rust-time-core-0.1.2
  (crate-source "time-core" "0.1.2"
                "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))

(define rust-time-macros-0.2.19
  (crate-source "time-macros" "0.2.19"
                "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18"))

(define rust-tiny-keccak-2.0.2
  (crate-source "tiny-keccak" "2.0.2"
                "0dq2x0hjffmixgyf6xv9wgsbcxkd65ld0wrfqmagji8a829kg79c"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinyvec-1.8.0
  (crate-source "tinyvec" "1.8.0"
                "0f5rf6a2wzyv6w4jmfga9iw7rp9fp5gf4d604xgjsf3d9wgqhpj4"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-titlecase-3.6.0
  (crate-source "titlecase" "3.6.0"
                "0h4xcxck5pvq6czki6idxdfhvqawpvi4k08caa9b8n8xm6470mpb"))

(define rust-tokio-1.46.1
  (crate-source "tokio" "1.46.1"
                "05sxldy7kcgysnxyzz1h1l8j3d9mjyqfh7r48ni27gmg9lsa5hqc"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tokio-rustls-0.26.1
  (crate-source "tokio-rustls" "0.26.1"
                "0dxz4bhkn4bwnvzjqvqlg70ba5fslnmf9r6yr87wzq5cx9shjvaz"))

(define rust-tokio-util-0.7.13
  (crate-source "tokio-util" "0.7.13"
                "0y0h10a52c7hrldmr3410bp7j3fadq0jn9nf7awddgd2an6smz6p"))

(define rust-toml-0.8.19
  (crate-source "toml" "0.8.19"
                "0knjd3mkxyb87qcs2dark3qkpadidap3frqfj5nqvhpxwfc1zvd1"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-edit-0.22.22
  (crate-source "toml_edit" "0.22.22"
                "1xf7sxfzmnc45f75x302qrn5aph52vc8w226v59yhrm211i8vr2a"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.20
  (crate-source "tracing-subscriber" "0.3.20"
                "1m9447bxq7236avgl6n5yb2aqwplrghm61dgipw03mh7ad7s2m10"))

(define rust-trash-5.2.1
  (crate-source "trash" "5.2.1"
                "0bzxzyjywb6sdyfshr9fp3fbpjf7gfhznh9ybrlb8rh3q9icmrd8"))

(define rust-tree-magic-mini-3.1.6
  (crate-source "tree_magic_mini" "3.1.6"
                "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))

(define rust-trim-in-place-0.1.7
  (crate-source "trim-in-place" "0.1.7"
                "1z04g79xkrpf3h4g3cc8wax72dn6h6v9l4m39zg8rg39qrpr4gil"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-type-map-0.5.1
  (crate-source "type-map" "0.5.1"
                "143v32wwgpymxfy4y8s694vyq0wdi7li4s5dmms5w59nj2yxnc6b"))

(define rust-typed-arena-1.7.0
  (crate-source "typed-arena" "1.7.0"
                "0va4q7439qzlxh9acd9nba7m7sljdh7xz1gp8l0i597b0y025cm9"))

(define rust-typeid-1.0.2
  (crate-source "typeid" "1.0.2"
                "0vi32jv3s3nbybbl4r317wi2bk8j4fx4d8p88jji8pnd1hpdn4qf"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-typetag-0.2.18
  (crate-source "typetag" "0.2.18"
                "1glv3pdj154jlgp702nb2qxr72rq0xjdiwj45i5hbq7zhrp3pfjj"))

(define rust-typetag-impl-0.2.18
  (crate-source "typetag-impl" "0.2.18"
                "10r42wlz1gddnq7x4fk6bf7h6dsdblb4zqz57k91r3rcqhi0mckh"))

(define rust-tz-rs-0.7.0
  (crate-source "tz-rs" "0.7.0"
                "18bi7k9zgwbm0ch049c1mj901a6aza4mr4z7f0hfg5wkp7r0nig1"))

(define rust-tzdb-0.7.2
  (crate-source "tzdb" "0.7.2"
                "1xgv84ipra42fvvv8fx5lvqjy0h9w72jbf608ygl95gjarcymqhb"))

(define rust-tzdb-data-0.2.1
  (crate-source "tzdb_data" "0.2.1"
                "0fw0zyxl0x4qfnqwljyjjn40g0crm5ssr30kvd7pf2ir3xfb6106"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-umask-2.1.0
  (crate-source "umask" "2.1.0"
                "071xszsd6znk0ik11pxl7mwhf07clsiq3qpzw1ac0dcyak14d6pc"))

(define rust-unic-langid-0.9.6
  (crate-source "unic-langid" "0.9.6"
                "01bx59sqsx2jz4z7ppxq9kldcjq9dzadkmb2dr7iyc85kcnab2x2"))

(define rust-unic-langid-impl-0.9.6
  (crate-source "unic-langid-impl" "0.9.6"
                "0n66kdan4cz99n8ra18i27f7w136hmppi4wc0aa7ljsd0h4bzqfw"))

(define rust-unicase-2.8.0
  (crate-source "unicase" "2.8.0"
                "1pznbipizi6yg481167c9mp0xgn1s4cv379pnr9zhmzihf0bclby"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-reverse-1.0.9
  (crate-source "unicode-reverse" "1.0.9"
                "0xhcybbgy0l8s8n7sfd6hxi854f8znlxqkspzfnr8c62xf44hvsb"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-truncate-1.1.0
  (crate-source "unicode-truncate" "1.1.0"
                "1gr7arjjhrhy8dww7hj8qqlws97xf9d276svr4hs6pxgllklcr5k"))

(define rust-unicode-width-0.1.11
  (crate-source "unicode-width" "0.1.11"
                "11ds4ydhg8g7l06rlmh712q41qsrd0j0h00n1jm74kww3kqk65z5"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unit-prefix-0.5.1
  (crate-source "unit-prefix" "0.5.1"
                "05rq0asf2f1q5vrcv4bwf0c3y6q20asqkiqpr8wqyrfxyb7h4d1j"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-unty-0.0.4
  (crate-source "unty" "0.0.4"
                "1blhyv01qiv5sb72sal3xa1l8nzck3answawxkkiw3fd2x1phjbd"))

(define rust-update-informer-1.3.0
  (crate-source "update-informer" "1.3.0"
                "0anf7a855m86hky2nlw337f5ay8sdri02lh8q9javikdfv7pvck7"))

(define rust-ureq-3.0.12
  (crate-source "ureq" "3.0.12"
                "0f8cfrigffa6r2y2jjig1ck3bkab6p5ng32z2n0y69hhr6dxw3wz"))

(define rust-ureq-proto-0.4.2
  (crate-source "ureq-proto" "0.4.2"
                "1xwkysnq4lq05vd8azyml1f60wxih3z83nmncazb3wi336npinsr"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uu-cp-0.2.2
  (crate-source "uu_cp" "0.2.2"
                "0b74khmpnsjx9fmq49lwsxj3kpma6vzls6s72m4fsb0369lp603r"))

(define rust-uu-mkdir-0.2.2
  (crate-source "uu_mkdir" "0.2.2"
                "1zfwqp8mf9frbx7a3waks8qmnp6p1rgfnc8j14b0qrvyhgjqidac"))

(define rust-uu-mktemp-0.2.2
  (crate-source "uu_mktemp" "0.2.2"
                "0385426gkvqcdr2jc7fkw243q6dl2rna2c3s03ycf1xzvghh3gza"))

(define rust-uu-mv-0.2.2
  (crate-source "uu_mv" "0.2.2"
                "0bi8cckqadxp4qwpzwvk4k57812j8zrkx2lsipxrsa6z0j0qcyzp"))

(define rust-uu-touch-0.2.2
  (crate-source "uu_touch" "0.2.2"
                "16llqz4xqy7yh75xwkkbpzmfxcqzphbpry1mhbwhf5zaws610ss5"))

(define rust-uu-uname-0.2.2
  (crate-source "uu_uname" "0.2.2"
                "08952ad5vidzk4g28d5v7aahzs6a4i3s9ws87k351w2ah3yqb7dh"))

(define rust-uu-whoami-0.2.2
  (crate-source "uu_whoami" "0.2.2"
                "0a2w7zdcw1azy8ndbd7k6rcdm84j27zkdjda20zb70hgz6bds8l7"))

(define rust-uucore-0.2.2
  (crate-source "uucore" "0.2.2"
                "1b081qmhvaddflkh5r0sx30px5ki9a5kscm5rd848d5ch27f80vj"))

(define rust-uucore-procs-0.2.2
  (crate-source "uucore_procs" "0.2.2"
                "1pjrimnrkrpayi6ljixn4z01xvbasc38ggcpb26crl3f277697j4"))

(define rust-uuhelp-parser-0.2.2
  (crate-source "uuhelp_parser" "0.2.2"
                "0icpkb7vn8i2bjcd9fplx2vcy83g7lzy5sjfvj2yi5hrc52lk7lw"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

(define rust-v-htmlescape-0.15.8
  (crate-source "v_htmlescape" "0.15.8"
                "135inp4x7cc32k0hzrymlz1baf0rj0ah5h82nrpa9w0hqpxmg0jf"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-value-trait-0.11.0
  (crate-source "value-trait" "0.11.0"
                "1f4nnc70zfb1zk4z27fkvq12ky7qqyzbc86f96mhm7ni3bhzq205"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-virtue-0.0.18
  (crate-source "virtue" "0.0.18"
                "1cgp79pzzs117kjlc3jnnkixbyaqri12j40mx2an41qhrymv27h5"))

(define rust-vsimd-0.8.0
  (crate-source "vsimd" "0.8.0"
                "0r4wn54jxb12r0x023r5yxcrqk785akmbddqkcafz9fm03584c2w"))

(define rust-vte-0.14.1
  (crate-source "vte" "0.14.1"
                "0xy01fgkzb2080prh2ncd8949hm2248fc5wf1lryhdrhxzbxq7r3"))

(define rust-wait-timeout-0.2.0
  (crate-source "wait-timeout" "0.2.0"
                "1xpkk0j5l9pfmjfh1pi0i89invlavfrd9av5xp0zhxgb29dhy84z"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.13.3+wasi-0.2.2
  (crate-source "wasi" "0.13.3+wasi-0.2.2"
                "1lnapbvdcvi3kc749wzqvwrpd483win2kicn1faa4dja38p6v096"))

(define rust-wasm-bindgen-0.2.101
  (crate-source "wasm-bindgen" "0.2.101"
                "0fv0yrfx170gf7i4dds4c69dxh8axp247wyip2dm4nylmmf9253y"))

(define rust-wasm-bindgen-backend-0.2.101
  (crate-source "wasm-bindgen-backend" "0.2.101"
                "1fwkzc2z701g2rm2jq4m20a0lkc6qqq5r3a407yj6yfahalip3g2"))

(define rust-wasm-bindgen-futures-0.4.51
  (crate-source "wasm-bindgen-futures" "0.4.51"
                "1znz8i8kyrlpq6q2fals223zrwwixmn6s7a16s1v6sdlm4wm1a0c"))

(define rust-wasm-bindgen-macro-0.2.101
  (crate-source "wasm-bindgen-macro" "0.2.101"
                "038vxk2yg11c3qv9iyasqcm70dw8sr2xmyaxqjq7bxzgwcx4cgbw"))

(define rust-wasm-bindgen-macro-support-0.2.101
  (crate-source "wasm-bindgen-macro-support" "0.2.101"
                "1ajjqmdbi7ybdpw41avskjfdqnxpc9v547gmr8izj4c2n24wxd3v"))

(define rust-wasm-bindgen-shared-0.2.101
  (crate-source "wasm-bindgen-shared" "0.2.101"
                "1h94nvm5p8zyr3718x4zhdz7rcmd0rir0b46a1ljqx8k7d58ahzi"))

(define rust-wasm-streams-0.4.2
  (crate-source "wasm-streams" "0.4.2"
                "0rddn007hp6k2cm91mm9y33n79b0jxv0c3znzszcvv67hn6ks18m"))

(define rust-wax-0.6.0
  (crate-source "wax" "0.6.0"
                "0mqk9qxsjlbwnjnj0gkaa29qm3mmgbgrc6pd4qpjvcmsl25af4ld"))

(define rust-wayland-backend-0.3.7
  (crate-source "wayland-backend" "0.3.7"
                "1xhnh0mn4cv0wmq3zcm0iic2sbhsz4qdra6kb58x8l51sz73ar85"))

(define rust-wayland-client-0.31.7
  (crate-source "wayland-client" "0.31.7"
                "105j23dj1k36rpvv3nk5v3lm99gs029k3k429kbnzxv9zk9ljqmn"))

(define rust-wayland-protocols-0.31.2
  (crate-source "wayland-protocols" "0.31.2"
                "1x310l1p6p3p3l76nl1l2yava9408dy77s605917zadlp1jz70cg"))

(define rust-wayland-protocols-wlr-0.2.0
  (crate-source "wayland-protocols-wlr" "0.2.0"
                "1mjww9psk2nc5hm2q4s3qas30rbzfg1sb6qgw518fbbcdfvn27xd"))

(define rust-wayland-scanner-0.31.5
  (crate-source "wayland-scanner" "0.31.5"
                "1hv16shy6j32hi9i0r54pyk5pw3q7qfpkffmwchi3z75n80j0zsr"))

(define rust-wayland-sys-0.31.5
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.5"
                "02cyl94ydazgjdjf7asm2phni8h62j4cg4pwr6sy7lwfiq6sra7g"))

(define rust-web-atoms-0.1.3
  (crate-source "web_atoms" "0.1.3"
                "056lg00xm67d2yiyi1fh3x15jpi3idk0acifk7wvsh0jq0fxxzsp"))

(define rust-web-sys-0.3.78
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.78"
                "04lbcdr74pilsrf1g76lbw9bwg7zghgslqxdiwmxkw4zfhvvdr3p"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpage-2.0.1
  (crate-source "webpage" "2.0.1"
                "1b1fh3k6xcwkksyi9gbcx28d15h5mqs9rfw2maxycihx0ky2x1kh"))

(define rust-webpki-root-certs-0.26.11
  (crate-source "webpki-root-certs" "0.26.11"
                "0gir7r0pb4lirqfx7gzgcw3pz3aj4hfmykri62svyvhlj7pz1ivm"))

(define rust-webpki-root-certs-1.0.1
  (crate-source "webpki-root-certs" "1.0.1"
                "03i83z7cibzms4qr0bjhvi1ml06xp0kp1rv98jy63mdpn8aqn4w6"))

(define rust-webpki-roots-0.26.8
  (crate-source "webpki-roots" "0.26.8"
                "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412"))

(define rust-webpki-roots-1.0.0
  (crate-source "webpki-roots" "1.0.0"
                "1nyg365shpxkbazrsvh9c4cv7ar16xnfq62w48xdmwn43j6p6lr8"))

(define rust-which-8.0.0
  (crate-source "which" "8.0.0"
                "07dsqyvvyaqp3dbj4cdl3ib5fxhdf29l6vihm3pcihq666avpynk"))

(define rust-widestring-1.1.0
  (crate-source "widestring" "1.1.0"
                "048kxd6iykzi5la9nikpc5hvpp77hmjf1sw43sl3z2dcdrmx66bj"))

(define rust-wild-2.2.1
  (crate-source "wild" "2.2.1"
                "1q8hnhmv3fvgx0j7bv8qig00599a15mfsdhgx3hq2ljpiky1l4x3"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-0.56.0
  (crate-source "windows" "0.56.0"
                "0cp10nzrqgrlk91dpwxjcpzyy6imr5vxr5f898pss7nz3gq9vrhx"))

(define rust-windows-0.61.3
  (crate-source "windows" "0.61.3"
                "14v8dln7i4ccskd8danzri22bkjkbmgzh284j3vaxhd4cykx7awv"))

(define rust-windows-0.62.1
  (crate-source "windows" "0.62.1"
                "1b5q2nay1xwilqyh3kzc86wny7668pv29fkpdw623j33yfhw9rj9"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-collections-0.3.1
  (crate-source "windows-collections" "0.3.1"
                "0bl5nxpipizmd3zl4n6bcijb0fnls93c9x0k3b7612ja8qpp2ghj"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.56.0
  (crate-source "windows-core" "0.56.0"
                "19pj57bm0rzhlk0ghrccd3i5zvh0ghm52f8cmdc8d3yhs8pfb626"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-core-0.62.1
  (crate-source "windows-core" "0.62.1"
                "1aa94x61q0x39xnlzxjmahwck9i5p51xgzrz7m6hi1dj2rafwi38"))

(define rust-windows-future-0.2.1
  (crate-source "windows-future" "0.2.1"
                "13mdzcdn51ckpzp3frb8glnmkyjr1c30ym9wnzj9zc97hkll2spw"))

(define rust-windows-future-0.3.1
  (crate-source "windows-future" "0.3.1"
                "0jlsi8bmsv0lfmq6yqnjm39qw62794dq3m4wch6j085i4imxpwv8"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-implement-0.56.0
  (crate-source "windows-implement" "0.56.0"
                "16rgkvlx4syqmajfdwmkcvn6nvh126wjj8sg3jvsk5fdivskbz7n"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.56.0
  (crate-source "windows-interface" "0.56.0"
                "1k2prfxna0mw47f8gi8qhw9jfpw66bh2cqzs67sgipjfpx30b688"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-numerics-0.3.0
  (crate-source "windows-numerics" "0.3.0"
                "1n237cl7c9lkdxj2f32yvdih6jvb361q6h3pwii1xa5bw27lkqrc"))

(define rust-windows-registry-0.2.0
  (crate-source "windows-registry" "0.2.0"
                "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-result-0.2.0
  (crate-source "windows-result" "0.2.0"
                "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-result-0.4.0
  (crate-source "windows-result" "0.4.0"
                "0zqn8kmmf7y9yw9g7q6pbcg9dbry9m03fqi0b92q767q0v1xr13h"))

(define rust-windows-strings-0.1.0
  (crate-source "windows-strings" "0.1.0"
                "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-strings-0.5.0
  (crate-source "windows-strings" "0.5.0"
                "1nld65azvms87rdm2bdm8gskwdmsswh4pxbc8babxc2klmawc63j"))

(define rust-windows-sys-0.45.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.61.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.0"
                "1ajpwsmzfcsa1r7i0dxzvfn24dp3525rcd7aq95ydvdj8171h0g2"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-threading-0.1.0
  (crate-source "windows-threading" "0.1.0"
                "19jpn37zpjj2q7pn07dpq0ay300w65qx7wdp13wbp8qf5snn6r5n"))

(define rust-windows-threading-0.2.0
  (crate-source "windows-threading" "0.2.0"
                "06718prxhs52310s6qlfskrf5l6db33m8ll593xdwck9mn2z0ixb"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-winnow-0.6.20
  (crate-source "winnow" "0.6.20"
                "16y4i8z9vh8hazjxg5mvmq0c5i35wlk8rxi5gkq6cn5vlb0zxh9n"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winreg-0.52.0
  (crate-source "winreg" "0.52.0"
                "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))

(define rust-winresource-0.1.19
  (crate-source "winresource" "0.1.19"
                "0fv9xlgg9a6gp1jhrp7zj7kln7ris64889n3z1x59m1s6ldnjxkj"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-rt-0.33.0
  (crate-source "wit-bindgen-rt" "0.33.0"
                "0g4lwfp9x6a2i1hgjn8k14nr4fsnpd5izxhc75zpi2s5cvcg6s1j"))

(define rust-wl-clipboard-rs-0.8.1
  (crate-source "wl-clipboard-rs" "0.8.1"
                "1nwa0bg6jpq5sd8x94xgkj0yk7zcz2m3sg2mm26b35qlj5rigd0j"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-xattr-1.3.1
  (crate-source "xattr" "1.3.1"
                "0kqxm36w89vc6qcpn6pizlhgjgzq138sx4hdhbv2g6wk4ld4za4d"))

(define rust-xml5ever-0.18.1
  (crate-source "xml5ever" "0.18.1"
                "0sdz92vrcxfwv7yzai28y0wa9gswr6msjnksak0rp4cfbm02dfwv"))

(define rust-xmlparser-0.13.6
  (crate-source "xmlparser" "0.13.6"
                "1r796g21c70p983ax0j6rmhzmalg4rhx61mvd4farxdhfyvy1zk6"))

(define rust-xxhash-rust-0.8.12
  (crate-source "xxhash-rust" "0.8.12"
                "1139skyp136z8710r916kb1djp7f7flfly31zccqi5800isvyp3a"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-0.8.23
  (crate-source "zerocopy" "0.8.23"
                "1inbxgqhsxghawsss8x8517g30fpp8s3ll2ywy88ncm40m6l95zx"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerocopy-derive-0.8.23
  (crate-source "zerocopy-derive" "0.8.23"
                "0m7iwisxz111sgkski722nyxv0rixbs0a9iylrcvhpfx1qfw0lk3"))

(define rust-zerofrom-0.1.5
  (crate-source "zerofrom" "0.1.5"
                "0bnd8vjcllzrvr3wvn8x14k2hkrpyy1fm3crkn2y3plmr44fxwyg"))

(define rust-zerofrom-derive-0.1.5
  (crate-source "zerofrom-derive" "0.1.5"
                "022q55phhb44qbrcfbc48k0b741fl8gnazw3hpmmndbx5ycfspjr"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zip-4.1.0
  (crate-source "zip" "4.1.0"
                "181raik3bx9m181lvyvlg90q1sd04rvxw90sag17j3lw4ascszdg"))

(define rust-zlib-rs-0.5.1
  (crate-source "zlib-rs" "0.5.1"
                "12nvshiq19nd4ksn3453ym9p0kcqf0hpaq301p2iyx9ljzxdjsv2"))

(define rust-zopfli-0.8.1
  (crate-source "zopfli" "0.8.1"
                "0ip9azz9ldk19m0m1hdppz3n5zcz0cywbg1vx59g4p5c3cwry0g5"))

(define rust-zstd-0.13.2
  (crate-source "zstd" "0.13.2"
                "1ygkr6wspm9clbp7ykyl0rv69cfsf9q4lic9wcqiwn34lrwbgwpw"))

(define rust-zstd-safe-7.2.1
  (crate-source "zstd-safe" "7.2.1"
                "0nch85m5cr493y26yvndm6a8j6sd9mxpr2awrim3dslcnr6sp8sl"))

(define rust-zstd-sys-2.0.13+zstd.1.5.6
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.13+zstd.1.5.6"
                "1almbackh06am0d2kc4a089n3al91jg3ahgg9kcrg3zfrwhhzzrq"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (nushell =>
                              (list rust-addr2line-0.24.2
                               rust-adler2-2.0.0
                               rust-adler32-1.2.0
                               rust-ahash-0.8.11
                               rust-aho-corasick-1.1.3
                               rust-alloc-no-stdlib-2.0.4
                               rust-alloc-stdlib-0.2.2
                               rust-alloca-0.4.0
                               rust-allocator-api2-0.2.21
                               rust-alphanumeric-sort-1.5.3
                               rust-android-system-properties-0.1.5
                               rust-ansi-str-0.9.0
                               rust-ansitok-0.3.0
                               rust-anstream-0.6.18
                               rust-anstyle-1.0.10
                               rust-anstyle-parse-0.2.6
                               rust-anstyle-query-1.1.2
                               rust-anstyle-wincon-3.0.6
                               rust-anyhow-1.0.94
                               rust-arbitrary-1.4.1
                               rust-arboard-3.4.1
                               rust-argminmax-0.6.3
                               rust-array-init-cursor-0.2.1
                               rust-arrayref-0.3.9
                               rust-arrayvec-0.7.6
                               rust-assert-json-diff-2.0.2
                               rust-assert-cmd-2.0.16
                               rust-async-channel-2.3.1
                               rust-async-stream-0.3.6
                               rust-async-stream-impl-0.3.6
                               rust-async-trait-0.1.83
                               rust-atoi-simd-0.16.0
                               rust-atomic-0.6.0
                               rust-atomic-waker-1.1.2
                               rust-autocfg-1.4.0
                               rust-avro-schema-0.3.0
                               rust-aws-config-1.5.10
                               rust-aws-credential-types-1.2.1
                               rust-aws-runtime-1.4.3
                               rust-aws-sdk-sso-1.49.0
                               rust-aws-sdk-ssooidc-1.50.0
                               rust-aws-sdk-sts-1.50.0
                               rust-aws-sigv4-1.2.5
                               rust-aws-smithy-async-1.2.1
                               rust-aws-smithy-http-0.60.11
                               rust-aws-smithy-json-0.60.7
                               rust-aws-smithy-query-0.60.7
                               rust-aws-smithy-runtime-1.7.3
                               rust-aws-smithy-runtime-api-1.7.3
                               rust-aws-smithy-types-1.2.9
                               rust-aws-smithy-xml-0.60.9
                               rust-aws-types-1.3.3
                               rust-backtrace-0.3.74
                               rust-backtrace-ext-0.2.1
                               rust-base64-0.21.7
                               rust-base64-0.22.1
                               rust-base64-simd-0.8.0
                               rust-base64ct-1.8.0
                               rust-bigdecimal-0.4.8
                               rust-bincode-2.0.1
                               rust-bincode-derive-2.0.1
                               rust-bindgen-0.70.1
                               rust-bit-set-0.8.0
                               rust-bit-vec-0.8.0
                               rust-bitflags-1.3.2
                               rust-bitflags-2.6.0
                               rust-blake3-1.8.2
                               rust-block-buffer-0.10.4
                               rust-block2-0.5.1
                               rust-boxcar-0.2.13
                               rust-bracoxide-0.1.6
                               rust-brotli-8.0.2
                               rust-brotli-decompressor-5.0.0
                               rust-bstr-1.11.1
                               rust-buf-trait-0.4.1
                               rust-bumpalo-3.16.0
                               rust-bytecount-0.6.8
                               rust-bytemuck-1.23.0
                               rust-bytemuck-derive-1.8.0
                               rust-byteorder-1.5.0
                               rust-bytes-1.10.0
                               rust-bytes-utils-0.1.4
                               rust-bytesize-2.1.0
                               rust-byteyarn-0.5.1
                               rust-calamine-0.28.0
                               rust-cassowary-0.3.0
                               rust-castaway-0.2.3
                               rust-cc-1.2.16
                               rust-cesu8-1.1.0
                               rust-cexpr-0.6.0
                               rust-cfg-if-1.0.0
                               rust-cfg-aliases-0.1.1
                               rust-cfg-aliases-0.2.1
                               rust-chardetng-0.1.17
                               rust-charset-0.1.5
                               rust-chrono-0.4.42
                               rust-chrono-humanize-0.2.3
                               rust-chrono-tz-0.10.0
                               rust-chrono-tz-build-0.4.0
                               rust-chumsky-0.9.3
                               rust-clang-sys-1.8.1
                               rust-clap-4.5.23
                               rust-clap-builder-4.5.23
                               rust-clap-derive-4.5.18
                               rust-clap-lex-0.7.4
                               rust-clipboard-win-5.4.0
                               rust-codepage-0.1.2
                               rust-colorchoice-1.0.3
                               rust-colorz-1.1.4
                               rust-combine-4.6.7
                               rust-comfy-table-7.1.3
                               rust-compact-str-0.8.0
                               rust-compact-str-0.9.0
                               rust-concurrent-queue-2.5.0
                               rust-console-0.15.8
                               rust-console-0.16.1
                               rust-const-random-0.1.18
                               rust-const-random-macro-0.1.16
                               rust-const-format-0.2.34
                               rust-const-format-proc-macros-0.2.34
                               rust-constant-time-eq-0.3.1
                               rust-cookie-0.18.1
                               rust-cookie-store-0.21.1
                               rust-core-foundation-0.9.4
                               rust-core-foundation-0.10.0
                               rust-core-foundation-sys-0.8.7
                               rust-cpufeatures-0.2.16
                               rust-crc-2.1.0
                               rust-crc-catalog-1.1.1
                               rust-crc32fast-1.4.2
                               rust-crossbeam-channel-0.5.15
                               rust-crossbeam-deque-0.8.5
                               rust-crossbeam-epoch-0.9.18
                               rust-crossbeam-queue-0.3.11
                               rust-crossbeam-utils-0.8.21
                               rust-crossterm-0.28.1
                               rust-crossterm-winapi-0.9.1
                               rust-crunchy-0.2.2
                               rust-crypto-common-0.1.6
                               rust-cssparser-0.35.0
                               rust-cssparser-macros-0.6.1
                               rust-csv-1.3.1
                               rust-csv-core-0.1.11
                               rust-ctrlc-3.4.5
                               rust-curl-0.4.47
                               rust-curl-sys-0.4.78+curl-8.11.0
                               rust-darling-0.20.10
                               rust-darling-0.21.0
                               rust-darling-core-0.20.10
                               rust-darling-core-0.21.0
                               rust-darling-macro-0.20.10
                               rust-darling-macro-0.21.0
                               rust-data-encoding-2.9.0
                               rust-der-0.7.10
                               rust-deranged-0.3.11
                               rust-derive-new-0.6.0
                               rust-derive-arbitrary-1.4.1
                               rust-derive-more-2.0.1
                               rust-derive-more-impl-2.0.1
                               rust-devicons-0.6.12
                               rust-dialoguer-0.11.0
                               rust-diff-0.1.13
                               rust-difflib-0.4.0
                               rust-digest-0.10.7
                               rust-dirs-6.0.0
                               rust-dirs-sys-0.5.0
                               rust-displaydoc-0.2.5
                               rust-dlib-0.5.2
                               rust-dlv-list-0.5.2
                               rust-doc-comment-0.3.3
                               rust-doctest-file-1.0.0
                               rust-document-features-0.2.11
                               rust-downcast-rs-1.2.1
                               rust-dtoa-1.0.9
                               rust-dtoa-short-0.3.5
                               rust-dtparse-2.0.1
                               rust-dunce-1.0.5
                               rust-dyn-clone-1.0.17
                               rust-ego-tree-0.10.0
                               rust-either-1.15.0
                               rust-eml-parser-0.1.4
                               rust-encode-unicode-0.3.6
                               rust-encode-unicode-1.0.0
                               rust-encoding-rs-0.8.35
                               rust-env-filter-0.1.2
                               rust-env-home-0.1.0
                               rust-env-logger-0.8.4
                               rust-env-logger-0.11.5
                               rust-equivalent-1.0.1
                               rust-erased-serde-0.4.5
                               rust-errno-0.3.10
                               rust-error-code-3.3.1
                               rust-etcetera-0.10.0
                               rust-ethnum-1.5.0
                               rust-event-listener-5.4.0
                               rust-event-listener-strategy-0.5.4
                               rust-fallible-iterator-0.3.0
                               rust-fallible-streaming-iterator-0.1.9
                               rust-fancy-regex-0.16.1
                               rust-fast-float2-0.2.3
                               rust-fastrand-2.3.0
                               rust-fd-lock-4.0.2
                               rust-file-id-0.2.2
                               rust-filesize-0.2.0
                               rust-filetime-0.2.25
                               rust-fixedbitset-0.4.2
                               rust-flate2-1.1.2
                               rust-float-cmp-0.10.0
                               rust-fluent-0.17.0
                               rust-fluent-bundle-0.16.0
                               rust-fluent-langneg-0.13.0
                               rust-fluent-syntax-0.12.0
                               rust-fluent-uri-0.1.4
                               rust-fnv-1.0.7
                               rust-foldhash-0.1.5
                               rust-foldhash-0.2.0
                               rust-foreign-types-0.3.2
                               rust-foreign-types-shared-0.1.1
                               rust-form-urlencoded-1.2.1
                               rust-fs4-0.13.1
                               rust-fs-extra-1.3.0
                               rust-fsevent-sys-4.1.0
                               rust-futf-0.1.5
                               rust-futures-0.3.31
                               rust-futures-channel-0.3.31
                               rust-futures-core-0.3.31
                               rust-futures-executor-0.3.31
                               rust-futures-io-0.3.31
                               rust-futures-macro-0.3.31
                               rust-futures-sink-0.3.31
                               rust-futures-task-0.3.31
                               rust-futures-util-0.3.31
                               rust-fuzzy-matcher-0.3.7
                               rust-fxhash-0.2.1
                               rust-generator-0.8.7
                               rust-generic-array-0.14.7
                               rust-gethostname-0.4.3
                               rust-getrandom-0.2.15
                               rust-getrandom-0.3.1
                               rust-gimli-0.31.1
                               rust-git2-0.20.0
                               rust-gjson-0.8.1
                               rust-glob-0.3.1
                               rust-glob-match-0.2.1
                               rust-goblin-0.7.1
                               rust-h2-0.3.26
                               rust-h2-0.4.7
                               rust-halfbrown-0.3.0
                               rust-hash32-0.3.1
                               rust-hashbrown-0.14.5
                               rust-hashbrown-0.15.5
                               rust-hashbrown-0.16.0
                               rust-hashlink-0.10.0
                               rust-heapless-0.9.1
                               rust-heck-0.5.0
                               rust-hex-0.4.3
                               rust-hmac-0.12.1
                               rust-home-0.5.9
                               rust-html5ever-0.27.0
                               rust-html5ever-0.35.0
                               rust-http-0.2.12
                               rust-http-1.3.1
                               rust-http-body-0.4.6
                               rust-http-body-1.0.1
                               rust-http-body-util-0.1.2
                               rust-httparse-1.9.5
                               rust-httpdate-1.0.3
                               rust-human-date-parser-0.3.1
                               rust-humantime-2.1.0
                               rust-hyper-0.14.31
                               rust-hyper-1.5.1
                               rust-hyper-rustls-0.24.2
                               rust-hyper-rustls-0.27.3
                               rust-hyper-tls-0.6.0
                               rust-hyper-util-0.1.10
                               rust-iana-time-zone-0.1.61
                               rust-iana-time-zone-haiku-0.1.2
                               rust-ical-0.11.0
                               rust-icu-collections-1.5.0
                               rust-icu-locid-1.5.0
                               rust-icu-locid-transform-1.5.0
                               rust-icu-locid-transform-data-1.5.0
                               rust-icu-normalizer-1.5.0
                               rust-icu-normalizer-data-1.5.0
                               rust-icu-properties-1.5.1
                               rust-icu-properties-data-1.5.0
                               rust-icu-provider-1.5.0
                               rust-icu-provider-macros-1.5.0
                               rust-ident-case-1.0.1
                               rust-idna-1.0.3
                               rust-idna-adapter-1.2.0
                               rust-indexmap-2.11.0
                               rust-indicatif-0.18.0
                               rust-indoc-2.0.6
                               rust-inotify-0.9.6
                               rust-inotify-sys-0.1.5
                               rust-instability-0.3.7
                               rust-interprocess-2.2.2
                               rust-intl-memoizer-0.5.3
                               rust-intl-pluralrules-7.0.2
                               rust-inventory-0.3.15
                               rust-io-uring-0.7.8
                               rust-ipnet-2.10.1
                               rust-is-docker-0.2.0
                               rust-is-wsl-0.4.0
                               rust-is-ci-1.2.0
                               rust-is-debug-1.1.0
                               rust-is-executable-1.0.4
                               rust-is-terminal-polyfill-1.70.1
                               rust-itertools-0.11.0
                               rust-itertools-0.13.0
                               rust-itertools-0.14.0
                               rust-itoa-1.0.14
                               rust-jni-0.21.1
                               rust-jni-sys-0.3.0
                               rust-jobserver-0.1.32
                               rust-js-sys-0.3.78
                               rust-jsonpath-lib-polars-vendor-0.0.1
                               rust-kqueue-1.0.8
                               rust-kqueue-sys-1.0.4
                               rust-lazy-static-1.5.0
                               rust-lean-string-0.5.0
                               rust-libc-0.2.174
                               rust-libflate-1.4.0
                               rust-libflate-lz77-1.2.0
                               rust-libgit2-sys-0.18.0+1.9.0
                               rust-libloading-0.8.6
                               rust-libm-0.2.11
                               rust-libproc-0.14.10
                               rust-libredox-0.1.3
                               rust-libsqlite3-sys-0.35.0
                               rust-libssh2-sys-0.3.0
                               rust-libz-rs-sys-0.5.1
                               rust-libz-sys-1.1.20
                               rust-linked-hash-map-0.5.6
                               rust-linux-raw-sys-0.4.14
                               rust-linux-raw-sys-0.9.4
                               rust-linux-raw-sys-0.11.0
                               rust-lipsum-0.9.1
                               rust-litemap-0.7.4
                               rust-litrs-0.4.1
                               rust-lock-api-0.4.12
                               rust-lockfree-object-pool-0.1.6
                               rust-log-0.4.27
                               rust-loom-0.7.2
                               rust-lru-0.12.5
                               rust-lscolors-0.20.0
                               rust-lsp-server-0.7.8
                               rust-lsp-textdocument-0.4.2
                               rust-lsp-types-0.97.0
                               rust-lz4-1.28.0
                               rust-lz4-sys-1.11.1+lz4-1.10.0
                               rust-mac-0.1.1
                               rust-mach2-0.4.3
                               rust-markup5ever-0.12.1
                               rust-markup5ever-0.35.0
                               rust-markup5ever-rcdom-0.3.0
                               rust-match-token-0.35.0
                               rust-matchers-0.2.0
                               rust-md-5-0.10.6
                               rust-memchr-2.7.4
                               rust-memmap2-0.9.5
                               rust-miette-7.6.0
                               rust-miette-derive-7.6.0
                               rust-mime-0.3.17
                               rust-mime-guess-2.0.5
                               rust-minimal-lexical-0.2.1
                               rust-miniz-oxide-0.8.9
                               rust-mio-0.8.11
                               rust-mio-1.0.3
                               rust-mockito-1.7.0
                               rust-moka-0.12.10
                               rust-multipart-rs-0.1.13
                               rust-native-tls-0.2.12
                               rust-new-debug-unreachable-1.0.6
                               rust-nix-0.28.0
                               rust-nix-0.29.0
                               rust-nix-0.30.1
                               rust-nom-7.1.3
                               rust-notify-6.1.1
                               rust-notify-debouncer-full-0.3.2
                               rust-now-0.1.3
                               rust-ntapi-0.4.1
                               rust-nu-ansi-term-0.50.3
                               rust-nucleo-matcher-0.3.1
                               rust-num-0.4.3
                               rust-num-bigint-0.4.6
                               rust-num-complex-0.4.6
                               rust-num-conv-0.1.0
                               rust-num-format-0.4.4
                               rust-num-integer-0.1.46
                               rust-num-iter-0.1.45
                               rust-num-rational-0.4.2
                               rust-num-traits-0.2.19
                               rust-num-threads-0.1.7
                               rust-number-prefix-0.4.0
                               rust-objc-sys-0.3.5
                               rust-objc2-0.5.2
                               rust-objc2-app-kit-0.2.2
                               rust-objc2-core-data-0.2.2
                               rust-objc2-core-foundation-0.3.1
                               rust-objc2-core-image-0.2.2
                               rust-objc2-encode-4.0.3
                               rust-objc2-foundation-0.2.2
                               rust-objc2-io-kit-0.3.1
                               rust-objc2-metal-0.2.2
                               rust-objc2-quartz-core-0.2.2
                               rust-object-0.36.5
                               rust-object-store-0.12.1
                               rust-oem-cp-2.1.0
                               rust-omnipath-0.1.6
                               rust-once-cell-1.21.3
                               rust-open-5.3.1
                               rust-openssl-0.10.72
                               rust-openssl-macros-0.1.1
                               rust-openssl-probe-0.1.5
                               rust-openssl-src-300.4.1+3.4.0
                               rust-openssl-sys-0.9.107
                               rust-option-ext-0.2.0
                               rust-ordered-multimap-0.7.3
                               rust-os-display-0.1.3
                               rust-os-pipe-1.2.1
                               rust-outref-0.5.1
                               rust-owo-colors-4.1.0
                               rust-papergrid-0.17.0
                               rust-parking-2.2.1
                               rust-parking-lot-0.12.3
                               rust-parking-lot-core-0.9.10
                               rust-parse-zoneinfo-0.3.1
                               rust-parse-datetime-0.11.0
                               rust-paste-1.0.15
                               rust-pathdiff-0.2.3
                               rust-pem-rfc7468-0.7.0
                               rust-percent-encoding-2.3.1
                               rust-peresil-0.3.0
                               rust-pest-2.7.15
                               rust-pest-consume-1.1.3
                               rust-pest-consume-macros-1.1.0
                               rust-pest-derive-2.7.15
                               rust-pest-generator-2.7.15
                               rust-pest-meta-2.7.15
                               rust-petgraph-0.6.5
                               rust-phf-0.11.2
                               rust-phf-codegen-0.11.2
                               rust-phf-generator-0.11.2
                               rust-phf-macros-0.11.2
                               rust-phf-shared-0.11.2
                               rust-pin-project-lite-0.2.15
                               rust-pin-utils-0.1.0
                               rust-pkg-config-0.3.31
                               rust-plain-0.2.3
                               rust-planus-1.1.1
                               rust-platform-info-2.0.4
                               rust-plist-1.8.0
                               rust-polars-0.51.0
                               rust-polars-arrow-0.51.0
                               rust-polars-arrow-format-0.2.0
                               rust-polars-compute-0.51.0
                               rust-polars-core-0.51.0
                               rust-polars-dtype-0.51.0
                               rust-polars-error-0.51.0
                               rust-polars-expr-0.51.0
                               rust-polars-io-0.51.0
                               rust-polars-json-0.51.0
                               rust-polars-lazy-0.51.0
                               rust-polars-mem-engine-0.51.0
                               rust-polars-ops-0.51.0
                               rust-polars-parquet-0.51.0
                               rust-polars-parquet-format-0.1.0
                               rust-polars-plan-0.51.0
                               rust-polars-row-0.51.0
                               rust-polars-schema-0.51.0
                               rust-polars-sql-0.51.0
                               rust-polars-stream-0.51.0
                               rust-polars-time-0.51.0
                               rust-polars-utils-0.51.0
                               rust-pori-0.0.0
                               rust-portable-atomic-1.10.0
                               rust-powerfmt-0.2.0
                               rust-ppv-lite86-0.2.20
                               rust-precomputed-hash-0.1.1
                               rust-predicates-3.1.2
                               rust-predicates-core-1.0.8
                               rust-predicates-tree-1.0.11
                               rust-pretty-assertions-1.4.1
                               rust-print-positions-0.6.1
                               rust-proc-macro-error-attr2-2.0.0
                               rust-proc-macro-error2-2.0.1
                               rust-proc-macro2-1.0.92
                               rust-procfs-0.17.0
                               rust-procfs-core-0.17.0
                               rust-psm-0.1.24
                               rust-pure-rust-locales-0.8.1
                               rust-pwd-1.4.0
                               rust-quick-error-1.2.3
                               rust-quick-xml-0.36.2
                               rust-quick-xml-0.37.5
                               rust-quick-xml-0.38.3
                               rust-quickcheck-1.0.3
                               rust-quickcheck-macros-1.1.0
                               rust-quinn-0.11.6
                               rust-quinn-proto-0.11.9
                               rust-quinn-udp-0.5.8
                               rust-quote-1.0.37
                               rust-quoted-printable-0.5.1
                               rust-rand-0.8.5
                               rust-rand-0.9.0
                               rust-rand-chacha-0.3.1
                               rust-rand-chacha-0.9.0
                               rust-rand-core-0.6.4
                               rust-rand-core-0.9.3
                               rust-rand-distr-0.5.1
                               rust-ratatui-0.29.0
                               rust-raw-cpuid-11.2.0
                               rust-rayon-1.11.0
                               rust-rayon-core-1.13.0
                               rust-recursive-0.1.1
                               rust-recursive-proc-macro-impl-0.1.1
                               rust-recvmsg-1.0.0
                               rust-redox-syscall-0.5.8
                               rust-redox-users-0.5.2
                               rust-reedline-0.43.0
                               rust-ref-cast-1.0.23
                               rust-ref-cast-impl-1.0.23
                               rust-regex-1.11.1
                               rust-regex-automata-0.4.9
                               rust-regex-lite-0.1.6
                               rust-regex-syntax-0.8.5
                               rust-relative-path-1.9.3
                               rust-reqwest-0.12.9
                               rust-rfc2047-decoder-1.0.6
                               rust-ring-0.17.13
                               rust-rle-decode-fast-1.0.3
                               rust-rmcp-0.8.1
                               rust-rmcp-macros-0.8.1
                               rust-rmp-0.8.14
                               rust-rmp-serde-1.3.0
                               rust-roxmltree-0.20.0
                               rust-rstest-0.23.0
                               rust-rstest-macros-0.23.0
                               rust-rstest-reuse-0.7.0
                               rust-rusqlite-0.37.0
                               rust-rust-embed-8.7.0
                               rust-rust-embed-impl-8.7.0
                               rust-rust-embed-utils-8.7.0
                               rust-rust-ini-0.21.1
                               rust-rust-decimal-1.36.0
                               rust-rustc-demangle-0.1.24
                               rust-rustc-hash-1.1.0
                               rust-rustc-hash-2.1.0
                               rust-rustc-version-0.4.1
                               rust-rustix-0.38.42
                               rust-rustix-1.0.7
                               rust-rustls-0.21.12
                               rust-rustls-0.23.28
                               rust-rustls-native-certs-0.6.3
                               rust-rustls-native-certs-0.8.1
                               rust-rustls-pemfile-1.0.4
                               rust-rustls-pemfile-2.2.0
                               rust-rustls-pki-types-1.12.0
                               rust-rustls-platform-verifier-0.5.3
                               rust-rustls-platform-verifier-android-0.1.1
                               rust-rustls-webpki-0.101.7
                               rust-rustls-webpki-0.103.3
                               rust-rustversion-1.0.18
                               rust-ryu-1.0.18
                               rust-same-file-1.0.6
                               rust-scc-2.4.0
                               rust-schannel-0.1.27
                               rust-schemars-1.0.4
                               rust-schemars-derive-1.0.4
                               rust-scoped-tls-1.0.1
                               rust-scopeguard-1.2.0
                               rust-scraper-0.24.0
                               rust-scroll-0.11.0
                               rust-scroll-derive-0.11.1
                               rust-sct-0.7.1
                               rust-sdd-3.0.10
                               rust-security-framework-2.11.1
                               rust-security-framework-3.0.1
                               rust-security-framework-sys-2.12.1
                               rust-selectors-0.31.0
                               rust-self-cell-1.2.0
                               rust-semver-1.0.23
                               rust-serde-1.0.225
                               rust-serde-core-1.0.225
                               rust-serde-derive-1.0.225
                               rust-serde-derive-internals-0.29.1
                               rust-serde-json-1.0.133
                               rust-serde-repr-0.1.19
                               rust-serde-spanned-0.6.8
                               rust-serde-stacker-0.1.14
                               rust-serde-urlencoded-0.7.1
                               rust-serde-yaml-0.9.34+deprecated
                               rust-serial-test-3.2.0
                               rust-serial-test-derive-3.2.0
                               rust-servo-arc-0.4.0
                               rust-sha1-smol-1.0.1
                               rust-sha2-0.10.8
                               rust-shadow-rs-1.4.0
                               rust-sharded-slab-0.1.7
                               rust-shell-words-1.1.0
                               rust-shlex-1.3.0
                               rust-signal-hook-0.3.17
                               rust-signal-hook-mio-0.2.4
                               rust-signal-hook-registry-1.4.2
                               rust-simd-adler32-0.3.7
                               rust-simd-json-0.15.1
                               rust-simdutf8-0.1.5
                               rust-similar-2.7.0
                               rust-simplelog-0.12.2
                               rust-siphasher-0.3.11
                               rust-skiplist-0.6.0
                               rust-slab-0.4.9
                               rust-slotmap-1.0.7
                               rust-smallvec-1.13.2
                               rust-snap-1.1.1
                               rust-socket2-0.5.8
                               rust-socks-0.3.4
                               rust-sqlparser-0.53.0
                               rust-stable-deref-trait-1.2.0
                               rust-stacker-0.1.17
                               rust-static-assertions-1.1.0
                               rust-streaming-decompression-0.1.2
                               rust-streaming-iterator-0.1.9
                               rust-strength-reduce-0.2.4
                               rust-string-cache-0.8.9
                               rust-string-cache-codegen-0.5.4
                               rust-strip-ansi-escapes-0.2.1
                               rust-strsim-0.11.1
                               rust-strum-0.26.3
                               rust-strum-macros-0.26.4
                               rust-strum-macros-0.27.2
                               rust-subtle-2.6.1
                               rust-supports-color-3.0.2
                               rust-supports-hyperlinks-3.1.0
                               rust-supports-unicode-3.0.0
                               rust-sxd-document-0.3.2
                               rust-sxd-xpath-0.4.2
                               rust-syn-1.0.109
                               rust-syn-2.0.90
                               rust-sync-wrapper-1.0.2
                               rust-synstructure-0.13.1
                               rust-sys-locale-0.3.2
                               rust-sysinfo-0.36.1
                               rust-tabled-0.20.0
                               rust-tagptr-0.2.0
                               rust-tango-bench-0.6.0
                               rust-tempfile-3.23.0
                               rust-tendril-0.4.3
                               rust-termcolor-1.4.1
                               rust-terminal-size-0.4.1
                               rust-termtree-0.4.1
                               rust-testing-table-0.3.0
                               rust-textwrap-0.16.1
                               rust-thiserror-1.0.69
                               rust-thiserror-2.0.12
                               rust-thiserror-impl-1.0.69
                               rust-thiserror-impl-2.0.12
                               rust-thread-local-1.1.8
                               rust-time-0.3.37
                               rust-time-core-0.1.2
                               rust-time-macros-0.2.19
                               rust-tiny-keccak-2.0.2
                               rust-tinystr-0.7.6
                               rust-tinystr-0.8.1
                               rust-tinyvec-1.8.0
                               rust-tinyvec-macros-0.1.1
                               rust-titlecase-3.6.0
                               rust-tokio-1.46.1
                               rust-tokio-macros-2.5.0
                               rust-tokio-native-tls-0.3.1
                               rust-tokio-rustls-0.24.1
                               rust-tokio-rustls-0.26.1
                               rust-tokio-util-0.7.13
                               rust-toml-0.8.19
                               rust-toml-datetime-0.6.8
                               rust-toml-edit-0.22.22
                               rust-tower-service-0.3.3
                               rust-tracing-0.1.41
                               rust-tracing-attributes-0.1.28
                               rust-tracing-core-0.1.33
                               rust-tracing-log-0.2.0
                               rust-tracing-subscriber-0.3.20
                               rust-trash-5.2.1
                               rust-tree-magic-mini-3.1.6
                               rust-trim-in-place-0.1.7
                               rust-try-lock-0.2.5
                               rust-type-map-0.5.1
                               rust-typed-arena-1.7.0
                               rust-typeid-1.0.2
                               rust-typenum-1.17.0
                               rust-typetag-0.2.18
                               rust-typetag-impl-0.2.18
                               rust-tz-rs-0.7.0
                               rust-tzdb-0.7.2
                               rust-tzdb-data-0.2.1
                               rust-ucd-trie-0.1.7
                               rust-umask-2.1.0
                               rust-unic-langid-0.9.6
                               rust-unic-langid-impl-0.9.6
                               rust-unicase-2.8.0
                               rust-unicode-ident-1.0.14
                               rust-unicode-linebreak-0.1.5
                               rust-unicode-normalization-0.1.24
                               rust-unicode-reverse-1.0.9
                               rust-unicode-segmentation-1.12.0
                               rust-unicode-truncate-1.1.0
                               rust-unicode-width-0.1.11
                               rust-unicode-width-0.2.0
                               rust-unicode-xid-0.2.6
                               rust-unit-prefix-0.5.1
                               rust-unsafe-libyaml-0.2.11
                               rust-untrusted-0.9.0
                               rust-unty-0.0.4
                               rust-update-informer-1.3.0
                               rust-ureq-3.0.12
                               rust-ureq-proto-0.4.2
                               rust-url-2.5.4
                               rust-urlencoding-2.1.3
                               rust-utf-8-0.7.6
                               rust-utf16-iter-1.0.5
                               rust-utf8-iter-1.0.4
                               rust-utf8parse-0.2.2
                               rust-uu-cp-0.2.2
                               rust-uu-mkdir-0.2.2
                               rust-uu-mktemp-0.2.2
                               rust-uu-mv-0.2.2
                               rust-uu-touch-0.2.2
                               rust-uu-uname-0.2.2
                               rust-uu-whoami-0.2.2
                               rust-uucore-0.2.2
                               rust-uucore-procs-0.2.2
                               rust-uuhelp-parser-0.2.2
                               rust-uuid-1.18.1
                               rust-v-htmlescape-0.15.8
                               rust-valuable-0.1.1
                               rust-value-trait-0.11.0
                               rust-vcpkg-0.2.15
                               rust-version-check-0.9.5
                               rust-virtue-0.0.18
                               rust-vsimd-0.8.0
                               rust-vte-0.14.1
                               rust-wait-timeout-0.2.0
                               rust-walkdir-2.5.0
                               rust-want-0.3.1
                               rust-wasi-0.11.0+wasi-snapshot-preview1
                               rust-wasi-0.13.3+wasi-0.2.2
                               rust-wasm-bindgen-0.2.101
                               rust-wasm-bindgen-backend-0.2.101
                               rust-wasm-bindgen-futures-0.4.51
                               rust-wasm-bindgen-macro-0.2.101
                               rust-wasm-bindgen-macro-support-0.2.101
                               rust-wasm-bindgen-shared-0.2.101
                               rust-wasm-streams-0.4.2
                               rust-wax-0.6.0
                               rust-wayland-backend-0.3.7
                               rust-wayland-client-0.31.7
                               rust-wayland-protocols-0.31.2
                               rust-wayland-protocols-wlr-0.2.0
                               rust-wayland-scanner-0.31.5
                               rust-wayland-sys-0.31.5
                               rust-web-sys-0.3.78
                               rust-web-time-1.1.0
                               rust-web-atoms-0.1.3
                               rust-webpage-2.0.1
                               rust-webpki-root-certs-0.26.11
                               rust-webpki-root-certs-1.0.1
                               rust-webpki-roots-0.26.8
                               rust-webpki-roots-1.0.0
                               rust-which-8.0.0
                               rust-widestring-1.1.0
                               rust-wild-2.2.1
                               rust-winapi-0.3.9
                               rust-winapi-i686-pc-windows-gnu-0.4.0
                               rust-winapi-util-0.1.9
                               rust-winapi-x86-64-pc-windows-gnu-0.4.0
                               rust-windows-0.56.0
                               rust-windows-0.61.3
                               rust-windows-0.62.1
                               rust-windows-collections-0.2.0
                               rust-windows-collections-0.3.1
                               rust-windows-core-0.52.0
                               rust-windows-core-0.56.0
                               rust-windows-core-0.61.2
                               rust-windows-core-0.62.1
                               rust-windows-future-0.2.1
                               rust-windows-future-0.3.1
                               rust-windows-implement-0.56.0
                               rust-windows-implement-0.60.2
                               rust-windows-interface-0.56.0
                               rust-windows-interface-0.59.3
                               rust-windows-link-0.1.3
                               rust-windows-link-0.2.0
                               rust-windows-numerics-0.2.0
                               rust-windows-numerics-0.3.0
                               rust-windows-registry-0.2.0
                               rust-windows-result-0.1.2
                               rust-windows-result-0.2.0
                               rust-windows-result-0.3.4
                               rust-windows-result-0.4.0
                               rust-windows-strings-0.1.0
                               rust-windows-strings-0.4.2
                               rust-windows-strings-0.5.0
                               rust-windows-sys-0.45.0
                               rust-windows-sys-0.48.0
                               rust-windows-sys-0.52.0
                               rust-windows-sys-0.59.0
                               rust-windows-sys-0.61.0
                               rust-windows-targets-0.42.2
                               rust-windows-targets-0.48.5
                               rust-windows-targets-0.52.6
                               rust-windows-threading-0.1.0
                               rust-windows-threading-0.2.0
                               rust-windows-aarch64-gnullvm-0.42.2
                               rust-windows-aarch64-gnullvm-0.48.5
                               rust-windows-aarch64-gnullvm-0.52.6
                               rust-windows-aarch64-msvc-0.42.2
                               rust-windows-aarch64-msvc-0.48.5
                               rust-windows-aarch64-msvc-0.52.6
                               rust-windows-i686-gnu-0.42.2
                               rust-windows-i686-gnu-0.48.5
                               rust-windows-i686-gnu-0.52.6
                               rust-windows-i686-gnullvm-0.52.6
                               rust-windows-i686-msvc-0.42.2
                               rust-windows-i686-msvc-0.48.5
                               rust-windows-i686-msvc-0.52.6
                               rust-windows-x86-64-gnu-0.42.2
                               rust-windows-x86-64-gnu-0.48.5
                               rust-windows-x86-64-gnu-0.52.6
                               rust-windows-x86-64-gnullvm-0.42.2
                               rust-windows-x86-64-gnullvm-0.48.5
                               rust-windows-x86-64-gnullvm-0.52.6
                               rust-windows-x86-64-msvc-0.42.2
                               rust-windows-x86-64-msvc-0.48.5
                               rust-windows-x86-64-msvc-0.52.6
                               rust-winnow-0.6.20
                               rust-winnow-0.7.13
                               rust-winreg-0.52.0
                               rust-winresource-0.1.19
                               rust-winsafe-0.0.19
                               rust-wit-bindgen-rt-0.33.0
                               rust-wl-clipboard-rs-0.8.1
                               rust-write16-1.0.0
                               rust-writeable-0.5.5
                               rust-x11rb-0.13.1
                               rust-x11rb-protocol-0.13.1
                               rust-xattr-1.3.1
                               rust-xml5ever-0.18.1
                               rust-xmlparser-0.13.6
                               rust-xxhash-rust-0.8.12
                               rust-yansi-1.0.1
                               rust-yoke-0.7.5
                               rust-yoke-derive-0.7.5
                               rust-zerocopy-0.7.35
                               rust-zerocopy-0.8.23
                               rust-zerocopy-derive-0.7.35
                               rust-zerocopy-derive-0.8.23
                               rust-zerofrom-0.1.5
                               rust-zerofrom-derive-0.1.5
                               rust-zeroize-1.8.1
                               rust-zerovec-0.10.4
                               rust-zerovec-0.11.4
                               rust-zerovec-derive-0.10.3
                               rust-zip-4.1.0
                               rust-zlib-rs-0.5.1
                               rust-zopfli-0.8.1
                               rust-zstd-0.13.2
                               rust-zstd-safe-7.2.1
                               rust-zstd-sys-2.0.13+zstd.1.5.6)))
