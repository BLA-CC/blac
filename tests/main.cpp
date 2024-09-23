#include <gtest/gtest.h>

TEST(test_suite_name, demo) {
    ASSERT_EQ(1, 1);
    ASSERT_TRUE(1);
    ASSERT_STREQ("hola", "hola");
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
