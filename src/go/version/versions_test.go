//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package version

import (
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestVersionsCompareVersions(t *testing.T) {
	assert.Equal(t, 0, Compare(SEMVER, nil, nil))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, nil, nil, nil))
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, nil, nil, false))
	assert.Equal(t, 0, Compare(SEMVER, strptr("1.0.0"), strptr("1.0.0")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("1.0.0"), nil))
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, strptr("1.0.0"), strptr("1.0.0"), false))
	assert.Equal(t, 0, Compare(SEMVER, strptr("1.2.3-alpha.1"), strptr("1.2.3-alpha.1")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("1.2.3-alpha.1"), strptr("1.2.3-alpha.1"), nil))
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, strptr("1.2.3-alpha.1"), strptr("1.2.3-alpha.1"), false))

	assert.True(t, Compare(SEMVER, strptr("1.0.0"), nil) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), nil, nil) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0"), nil, false) > 0)
	assert.True(t, Compare(SEMVER, strptr("1.0.0"), strptr("0.1.0")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("0.1.0"), nil) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0"), strptr("0.1.0"), false) > 0)
	assert.True(t, Compare(SEMVER, strptr("1.0.0"), strptr("1.0.0-alpha.1")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("1.0.0-alpha.1"), nil) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0"), strptr("1.0.0-alpha.1"), false) > 0)
	assert.True(t, Compare(SEMVER, strptr("1.0.0-alpha.2"), strptr("1.0.0-alpha.1")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.2"), strptr("1.0.0-alpha.1"), nil) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0-alpha.2"), strptr("1.0.0-alpha.1"), false) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0-alpha.10"), strptr("1.0.0-alpha.9"), false) > 0)

	assert.True(t, Compare(SEMVER, nil, strptr("1.0.0")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, nil, strptr("1.0.0"), nil) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, nil, strptr("1.0.0"), false) < 0)
	assert.True(t, Compare(SEMVER, strptr("0.1.0"), strptr("1.0.0")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("0.1.0"), strptr("1.0.0"), nil) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("0.1.0"), strptr("1.0.0"), false) < 0)
	assert.True(t, Compare(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0"), nil) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0"), false) < 0)
	assert.True(t, Compare(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0-alpha.2")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0-alpha.2"), nil) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0-alpha.2"), false) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.0-alpha.9"), strptr("1.0.0-alpha.10"), false) < 0)
}

func TestVersionsCompareAndSanitizeVersions(t *testing.T) {
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, nil, nil, true))
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, strptr("1.00.0"), strptr("01.0.0"), true))
	assert.Equal(t, 0, CompareWithSanitization(SEMVER, strptr("01.2.3-alpha.1"), strptr("1.2.3-alpha.1"), true))

	assert.True(t, CompareWithSanitization(SEMVER, strptr("01.0.0"), nil, true) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("01.0.0"), strptr("0.1.00"), true) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("01.0.0"), strptr("1.0.00-alpha.1"), true) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.00-alpha.2"), strptr("1.0.00-alpha.1"), true) > 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.0.00-alpha.010"), strptr("1.0.00-alpha.9"), true) > 0)

	assert.True(t, CompareWithSanitization(SEMVER, nil, strptr("01.0.0"), true) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("0.1.00"), strptr("01.0.000"), true) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("1.00.0-alpha.1"), strptr("01.0.0"), true) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("01.0.0-alpha.1"), strptr("1.0.00-alpha.2"), true) < 0)
	assert.True(t, CompareWithSanitization(SEMVER, strptr("01.0.0-alpha.9"), strptr("1.0.00-alpha.010"), true) < 0)
}

func TestVersionsCompareVersionsWithPrefix(t *testing.T) {
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, nil, nil, strptr("rel-")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("1.0.0"), strptr("rel-")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("rel-1.0.0"), strptr("1.0.0"), strptr("rel-")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("rel-1.0.0"), strptr("rel-")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("rel-1.2.3-alpha.1"), strptr("1.2.3-alpha.1"), strptr("rel-")))
	assert.Equal(t, 0, CompareWithPrefix(SEMVER, strptr("1.2.3-alpha.1"), strptr("rel-1.2.3-alpha.1"), strptr("rel-")))

	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), nil, strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0"), nil, strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("0.1.0"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0"), strptr("0.1.0"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("rel-0.1.0"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0"), strptr("1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0"), strptr("rel-1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.2"), strptr("1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0-alpha.2"), strptr("1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.2"), strptr("rel-1.0.0-alpha.1"), strptr("rel-")) > 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.10"), strptr("rel-1.0.0-alpha.9"), strptr("rel-")) > 0)

	assert.True(t, CompareWithPrefix(SEMVER, nil, strptr("1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, nil, strptr("rel-1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("0.1.0"), strptr("1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("0.1.0"), strptr("rel-1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-0.1.0"), strptr("1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("rel-1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0-alpha.1"), strptr("1.0.0"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("1.0.0-alpha.2"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("1.0.0-alpha.1"), strptr("rel-1.0.0-alpha.2"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0-alpha.1"), strptr("1.0.0-alpha.2"), strptr("rel-")) < 0)
	assert.True(t, CompareWithPrefix(SEMVER, strptr("rel-1.0.0-alpha.9"), strptr("1.0.0-alpha.10"), strptr("rel-")) < 0)
}

func TestVersionsDefaultInitial(t *testing.T) {
	assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, DefaultInitial(SEMVER).String())
}

func TestVersionsIsCoreWithEmptyString(t *testing.T) {
	assert.False(t, IsCore(SEMVER, ""))
}

func TestVersionsIsCoreWithInvalidVersion(t *testing.T) {
	for _, vv := range wellKnownInvalidVersions {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, IsCore(SEMVER, vv.version))
		})
	}
}

func TestVersionsIsCoreValidString(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsCore(SEMVER, *vv.version))
		})
	}
}

func TestVersionsIsNotCoreValidString(t *testing.T) {
	for _, vv := range wellKnownValidNonCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.False(t, IsCore(SEMVER, *vv.version))
		})
	}
}

func TestVersionsIsCoreValidStringWithPrefix(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsCoreWithPrefix(SEMVER, ""+*vv.version, nil))
			assert.True(t, IsCoreWithPrefix(SEMVER, ""+*vv.version, strptr("")))
			assert.True(t, IsCoreWithPrefix(SEMVER, "v"+*vv.version, strptr("v")))
			assert.True(t, IsCoreWithPrefix(SEMVER, "prefix"+*vv.version, strptr("prefix")))

			assert.False(t, IsCoreWithPrefix(SEMVER, "v"+*vv.version, nil))
			assert.False(t, IsCoreWithPrefix(SEMVER, "v"+*vv.version, strptr("")))
			assert.False(t, IsCoreWithPrefix(SEMVER, "prefix"+*vv.version, nil))
			assert.False(t, IsCoreWithPrefix(SEMVER, "prefix"+*vv.version, strptr("")))
		})
	}
}

func TestVersionsIsCoreSanitizedString(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsCoreWithLenience(SEMVER, *vv.version, true))
		})
	}
}

func TestVersionsIsCoreSanitizableString(t *testing.T) {
	for _, vv := range wellKnownSanitizableCoreVersions {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, IsCore(SEMVER, vv.version))
			assert.False(t, IsCoreWithLenience(SEMVER, vv.version, false))
			assert.True(t, IsCoreWithLenience(SEMVER, vv.version, true))
		})
	}
}

func TestVersionsIsLegalWithEmptyString(t *testing.T) {
	assert.False(t, IsLegal(SEMVER, ""))
}

func TestVersionsIsLegalWithInvalidVersion(t *testing.T) {
	for _, vv := range wellKnownInvalidVersions {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, IsLegal(SEMVER, vv.version))
		})
	}
}

func TestVersionsIsLegalValidString(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsLegal(SEMVER, *vv.version))
		})
	}
}

func TestVersionsIsLegalValidStringWithPrefix(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsLegalWithPrefix(SEMVER, ""+*vv.version, nil))
			assert.True(t, IsLegalWithPrefix(SEMVER, ""+*vv.version, strptr("")))
			assert.True(t, IsLegalWithPrefix(SEMVER, "v"+*vv.version, strptr("v")))
			assert.True(t, IsLegalWithPrefix(SEMVER, "prefix"+*vv.version, strptr("prefix")))

			assert.False(t, IsLegalWithPrefix(SEMVER, "v"+*vv.version, nil))
			assert.False(t, IsLegalWithPrefix(SEMVER, "v"+*vv.version, strptr("")))
			assert.False(t, IsLegalWithPrefix(SEMVER, "prefix"+*vv.version, nil))
			assert.False(t, IsLegalWithPrefix(SEMVER, "prefix"+*vv.version, strptr("")))
		})
	}
}

func TestVersionsIsLegalSanitizedString(t *testing.T) {
	for _, vv := range wellKnownValidCoreVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsLegalWithLenience(SEMVER, *vv.version, true))
		})
	}
}

func TestVersionsIsLegalSanitizableString(t *testing.T) {
	for _, vv := range wellKnownSanitizableCoreVersions {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, IsLegal(SEMVER, vv.version))
			assert.False(t, IsLegalWithLenience(SEMVER, vv.version, false))
			assert.True(t, IsLegalWithLenience(SEMVER, vv.version, true))
		})
	}
}

func TestVersionsMostRelevantIdentifier1(t *testing.T) {
	assert.Nil(t, MostRelevantIdentifierIn(SEMVER, []string{}))

	assert.Equal(t, "alpha", *MostRelevantIdentifierIn(SEMVER, []string{"alpha"}))

	assert.Equal(t, "alpha", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta"}))

	assert.Equal(t, "patch", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch"}))

	assert.Equal(t, "minor", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch", "minor"}))

	assert.Equal(t, "major", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch", "minor", "major"}))

	assert.Equal(t, "major", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch", "minor", "major", "gamma"}))

	assert.Equal(t, "major", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch", "minor", "major", "gamma", "theta"}))

	assert.Equal(t, "major", *MostRelevantIdentifierIn(SEMVER, []string{"alpha", "beta", "patch", "minor", "major", "gamma", "theta", "epsylon"}))
}

func TestVersionsMostRelevantIdentifier2(t *testing.T) {
	assert.Nil(t, MostRelevantIdentifierBetween(SEMVER, nil, nil))

	assert.Equal(t, "alpha", *MostRelevantIdentifierBetween(SEMVER, strptr("alpha"), nil))
	assert.Equal(t, "alpha", *MostRelevantIdentifierBetween(SEMVER, nil, strptr("alpha")))
	assert.Equal(t, "alpha", *MostRelevantIdentifierBetween(SEMVER, strptr("alpha"), strptr("alpha")))

	assert.Equal(t, "alpha", *MostRelevantIdentifierBetween(SEMVER, strptr("alpha"), strptr("beta")))
	assert.Equal(t, "alpha", *MostRelevantIdentifierBetween(SEMVER, strptr("beta"), strptr("alpha")))

	assert.Equal(t, "major", *MostRelevantIdentifierBetween(SEMVER, strptr("major"), strptr("major")))
	assert.Equal(t, "major", *MostRelevantIdentifierBetween(SEMVER, strptr("major"), strptr("minor")))
	assert.Equal(t, "major", *MostRelevantIdentifierBetween(SEMVER, strptr("major"), strptr("patch")))
	assert.Equal(t, "major", *MostRelevantIdentifierBetween(SEMVER, strptr("minor"), strptr("major")))
	assert.Equal(t, "major", *MostRelevantIdentifierBetween(SEMVER, strptr("patch"), strptr("major")))

	assert.Equal(t, "minor", *MostRelevantIdentifierBetween(SEMVER, strptr("minor"), strptr("minor")))
	assert.Equal(t, "minor", *MostRelevantIdentifierBetween(SEMVER, strptr("minor"), strptr("patch")))
	assert.Equal(t, "minor", *MostRelevantIdentifierBetween(SEMVER, strptr("patch"), strptr("minor")))

	assert.Equal(t, "patch", *MostRelevantIdentifierBetween(SEMVER, strptr("patch"), strptr("patch")))
	assert.Equal(t, "patch", *MostRelevantIdentifierBetween(SEMVER, strptr("patch"), strptr("alpha")))
	assert.Equal(t, "patch", *MostRelevantIdentifierBetween(SEMVER, strptr("alpha"), strptr("patch")))
}

func TestVersionsErrorUsingValueOfWithEmptyString(t *testing.T) {
	_, err := ValueOf(SEMVER, "")
	assert.Error(t, err)
}

func TestVersionsErrorUsingValueOfWithInvalidVersion(t *testing.T) {
	for _, vv := range wellKnownInvalidVersions {
		t.Run(vv.version, func(t *testing.T) {
			_, err := ValueOf(SEMVER, vv.version)
			assert.Error(t, err)
		})
	}
}

func TestVersionsValueOfValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOf(SEMVER, *vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestVersionsValueOfValidStringWithPrefix(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfWithPrefix(SEMVER, ""+*vv.version, nil)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())

			sv, err = ValueOfWithPrefix(SEMVER, ""+*vv.version, strptr(""))
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())

			sv, err = ValueOfWithPrefix(SEMVER, "v"+*vv.version, strptr("v"))
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())

			sv, err = ValueOfWithPrefix(SEMVER, "prefix"+*vv.version, strptr("prefix"))
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())

			sv, err = ValueOfWithPrefix(SEMVER, "v"+*vv.version, nil)
			assert.Error(t, err)

			sv, err = ValueOfWithPrefix(SEMVER, "v"+*vv.version, strptr(""))
			assert.Error(t, err)

			sv, err = ValueOfWithPrefix(SEMVER, "prefix"+*vv.version, nil)
			assert.Error(t, err)

			sv, err = ValueOfWithPrefix(SEMVER, "prefix"+*vv.version, strptr(""))
			assert.Error(t, err)
		})
	}
}

func TestVersionsValueOfSanitizedString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfWithSanitization(SEMVER, ""+*vv.version, true)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestVersionsValueOfSanitizableString(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			_, err := ValueOf(SEMVER, ""+vv.version)
			assert.Error(t, err)

			_, err = ValueOfWithSanitization(SEMVER, ""+vv.version, false)
			assert.Error(t, err)

			sv, err := ValueOfWithSanitization(SEMVER, ""+vv.version, true)
			assert.NoError(t, err)
			assert.NotEqual(t, vv.version, sv.String())

			sv, err = ValueOfWithSanitization(SEMVER, vv.version, true)
			assert.NoError(t, err)
			ss, err := SanitizeSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.Equal(t, ss, sv.String())
		})
	}
}
