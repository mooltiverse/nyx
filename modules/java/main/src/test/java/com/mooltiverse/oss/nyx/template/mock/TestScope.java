package com.mooltiverse.oss.nyx.template.mock;

import java.util.Arrays;
import java.util.List;

/**
 * A test class use as a scope to render templates.
 */
public class TestScope {
    public List<TestItem> items() {
        return Arrays.asList(
            new TestItem("Item 1", "$19.99", Arrays.asList(new TestFeature("New!"), new TestFeature("Awesome!"))),
            new TestItem("Item 2", "$29.99", Arrays.asList(new TestFeature("Old."), new TestFeature("Ugly.")))
        );
    }

    public TestScope() {
        super();
    }

    public static class TestItem {
        String name = null;
        String price = null;
        List<TestFeature> features;

        public TestItem(String name, String price, List<TestFeature> features) {
            this.name = name;
            this.price = price;
            this.features = features;
        }
    }

    public static class TestFeature {
        String description = null;

        public TestFeature(String description) {
            this.description = description;
        }
    }
}