package com.mooltiverse.oss.nyx.version;

/**
 * The implementation of a Maven compliant version.
 *
 * TODO implement this class. As of now it's a placeholder
 */
abstract class MavenVersion extends AbstractVersion implements Comparable<MavenVersion> {
    private MavenVersion() {
        super(null);
    }
}
