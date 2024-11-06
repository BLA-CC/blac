#include <gtest/gtest.h>

#include <vector>

#include "args.h"

#define test_name                                                              \
    (testing::UnitTest::GetInstance()->current_test_info()->name())

TEST(ArgTest, DefaultArgs) {
    // clang-format off
    std::vector<const char *> argv{
        test_name,
        "dummy.ctds",
    };
    // clang-format on

    Args args = arg_parse(argv.size(), argv.data());
    ASSERT_EQ(args.debug, false);
    ASSERT_EQ(args.targets, TARGET_DEFAULT);
    ASSERT_STREQ(args.output, OUTPUT_DEFAULT);
    ASSERT_STREQ(args.opt, OPTIMIZE_DEFAULT);
}

TEST(ArgTest, NamedArgs) {
    // clang-format off
    std::vector<const char *> argv{
        test_name,
        "-d",
        "-t",
        "ir",
        "-o",
        "hola",
        "dummy.ctds",
    };
    // clang-format on

    Args args = arg_parse(argv.size(), argv.data());
    ASSERT_TRUE(args.debug);
    ASSERT_STREQ(args.output, "hola");
    ASSERT_STREQ(args.input, "dummy.ctds");

    // makes sure IR is the only target
    ASSERT_TRUE((args.targets ^ TARGET_FLAG_IR) == 0);
}
