#包含 <stdio.h>

/* Define architecture */
#如 已定义(__i386__) || 已定义 _M_IX86
# 定义 TRIPLET_ARCH "i386"
#另如 已定义(__x86_64__) || 已定义 _M_AMD64
# 定义 TRIPLET_ARCH "x86_64"
#另如 已定义(__arm__)
# 定义 TRIPLET_ARCH "arm"
#另如 已定义(__aarch64__)
# 定义 TRIPLET_ARCH "aarch64"
#另
# 定义 TRIPLET_ARCH "unknown"
#了如

/* Define OS */
#如 已定义 (__linux__)
# 定义 TRIPLET_OS "linux"
#另如 已定义 (__FreeBSD__) || 已定义 (__FreeBSD_kernel__)
# 定义 TRIPLET_OS "kfreebsd"
#另如 已定义 _WIN32
# 定义 TRIPLET_OS "win32"
#另如 !已定义 (__GNU__)
# 定义 TRIPLET_OS "unknown"
#了如

/* Define calling convention and ABI */
#如 已定义 (__ARM_EABI__)
# 如 已定义 (__ARM_PCS_VFP)
#  定义 TRIPLET_ABI "gnueabihf"
# 另
#  定义 TRIPLET_ABI "gnueabi"
# 了如
#另
# 定义 TRIPLET_ABI "gnu"
#了如

#如 已定义 _WIN32
# 定义 TRIPLET TRIPLET_ARCH "-" TRIPLET_OS
#另如 已定义 __GNU__
# 定义 TRIPLET TRIPLET_ARCH "-" TRIPLET_ABI
#另
# 定义 TRIPLET TRIPLET_ARCH "-" TRIPLET_OS "-" TRIPLET_ABI
#了如

#如 已定义(_WIN32)
整 _CRT_glob = 0;
#了如

整 main(整 argc, 字 *argv[])
{
    转接(argc == 2 ? argv[1][0] : 0) {
        事例 'b':
        {
            易变 无符 foo = 0x01234567;
            puts(*(无符 字*)&foo == 0x67 ? "no" : "yes");
            跳出;
        }
#如定义 __GNUC__
        事例 'm':
            printf("%d\n", __GNUC_MINOR__);
            跳出;
        事例 'v':
            printf("%d\n", __GNUC__);
            跳出;
#另如 已定义 __TINYC__
        事例 'v':
            puts("0");
            跳出;
        事例 'm':
            printf("%d\n", __TINYC__);
            跳出;
#另
        事例 'm':
        事例 'v':
            puts("0");
            跳出;
#了如
        事例 't':
            puts(TRIPLET);
            跳出;

        缺省:
            跳出;
    }
    返回 0;
}
