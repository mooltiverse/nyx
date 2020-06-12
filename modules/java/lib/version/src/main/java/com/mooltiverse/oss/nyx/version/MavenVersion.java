package com.mooltiverse.oss.nyx.version;

/**
 * The implementation of a Maven compliant version.
 *
 * TODO implement this class. As of now it's a placeholder
 */
abstract class MavenVersion extends AbstractVersion implements Comparable<MavenVersion> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    private MavenVersion() {
        super(null);
    }
}
