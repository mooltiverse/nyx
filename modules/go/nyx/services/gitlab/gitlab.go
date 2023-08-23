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

/*
This is the GitLab package for Nyx, providing services to work with the GitLab service.
*/
package gitlab

import (
	"fmt"     // https://pkg.go.dev/fmt
	"net/url" // https://pkg.go.dev/net/url
	"os"      // https://pkg.go.dev/os
	"strings" // https://pkg.go.dev/strings

	log "github.com/sirupsen/logrus" // https://github.com/Sirupsen/logrus, https://pkg.go.dev/github.com/sirupsen/logrus
	gl "github.com/xanzy/go-gitlab"  // https://pkg.go.dev/github.com/xanzy/go-gitlab

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	api "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
)

const (
	/*
		The name of the option used to pass the base URI to this object instance.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the default URI is used.
	*/
	BASE_URI_OPTION_NAME = "BASE_URI"

	/*
		The name of the option used to pass the authentication token (Personal Access Token, OAuth)
		to this object instance.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to authenticate and perform any
		of the authentication protected operations.
	*/
	AUTHENTICATION_TOKEN_OPTION_NAME = "AUTHENTICATION_TOKEN"

	/*
		The name of the option used to pass the list of supported remotes to this object instance.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to perform some of its operations.
	*/
	REMOTES_OPTION_NAME = "REMOTES"

	/*
		The name of the option used to pass the name of the Git repository to this object instance.
		If the repository is https://gitlab.com/jdoe/project, the value to pass for
		this option is 'project', while if it's a hierarchical project like
		https://gitlab.com/acme/project/at/some/depth, the value to pass for
		this option is project/at/some/depth. Leading and trailing slashes must be omitted, if any.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to perform some of its operations.
	*/
	REPOSITORY_NAME_OPTION_NAME = "REPOSITORY_NAME"

	/*
		The name of the option used to pass the owner of the Git repository to this object instance.
		If the repository is https://gitlab.com/jdoe/project, the value to pass for
		this option is 'jdoe', and if it's a hierarchical project like
		https://gitlab.com/acme/project/at/some/depth, the value to pass for
		this option is acme. Leading and trailing slashes must be omitted, if any.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to perform some of its operations.
	*/
	REPOSITORY_OWNER_OPTION_NAME = "REPOSITORY_OWNER"
)

/*
The entry point to the GitLab remote service.
*/
type GitLab struct {
	// The name of the repository owner, used when using APIs that require the name of the repository
	// owner (individual or organization). It may be nil, but some operations may fail.
	repositoryOwner *string

	// The name of the repository, used when using APIs that require the name of the repository.
	// It may be nil, but some operations may fail.
	repositoryName *string

	// The private API client instance.
	client gl.Client
}

/*
Builds an instance using the given API.

Arguments are as follows:

  - client the API client to be used internally
  - repositoryOwner the name of the repository owner, used when using APIs that require the
    name of the repository owner (individual or organization). It may be nil, but some operations may fail
  - repositoryName the name of the repository, used when using APIs that require the
    name of the repository (simple or hierarchical, separated by '/'). It may be nil, but some operations may fail

Errors can be:

- NilPointerError if the given API is nil
*/
func newGitLab(client gl.Client, repositoryOwner *string, repositoryName *string) (GitLab, error) {
	res := GitLab{}
	res.client = client
	res.repositoryOwner = repositoryOwner
	res.repositoryName = repositoryName
	return res, nil
}

/*
Returns a new GitLab client instance.

Arguments are as follows:

- baseURI the custom endpoint to use (for private GitLab instances). If nil or empty the standard endpoint will be used
- authenticationToken the authentication token to use. If nil or empty no authentication is used

Errors can be returned by the underlying implementation
*/
func newClientInstance(baseURI *string, authenticationToken *string) (gl.Client, error) {
	log.Tracef("instantiating new GitLab client")
	token := ""
	if authenticationToken != nil && "" != strings.TrimSpace(*authenticationToken) {
		log.Debugf("the new GitLab service will use the given authentication token")
		token = *authenticationToken
	} else {
		log.Debugf("the new GitLab service does not use authentication because no token was passed")
	}

	if baseURI != nil && "" != strings.TrimSpace(*baseURI) {
		log.Tracef("the new GitLab service uses the custom URI '%s'", *baseURI)
		client, err := gl.NewClient(token, gl.WithBaseURL(*baseURI))
		return *client, err

	} else {
		log.Tracef("the new GitLab service uses the default URI")
		client, err := gl.NewClient(token)
		return *client, err
	}
}

/*
Returns an instance using the given options.

Arguments are as follows:

  - options the map of options for the requested service. It can't be nil.
    Valid options are documented as constants on this class.

Errors can be:

- NilPointerError if the given options map is nil
- IllegalArgumentError if some entries in the given options map are missing or illegal for some reason
*/
func Instance(options map[string]string) (GitLab, error) {
	if options == nil {
		return GitLab{}, &errs.NilPointerError{Message: fmt.Sprintf("can't create a new instance with a null options map")}
	}

	uriString, ok := options[BASE_URI_OPTION_NAME]
	if !ok {
		log.Debugf("no custom URI passed to the '%s' service, the default endpoint will be used", "GitLab")
	}
	authenticationToken, ok := options[AUTHENTICATION_TOKEN_OPTION_NAME]
	if !ok {
		log.Warnf("no authentication token passed to the '%s' service, no authentication protected operation will be available. Use the '%s' option to set this value", "GitLab", AUTHENTICATION_TOKEN_OPTION_NAME)
	}
	repositoryName, ok := options[REPOSITORY_NAME_OPTION_NAME]
	if !ok {
		log.Warnf("no repository name passed to the '%s' service, some features may not work. Use the '%s' option to set this value", "GitLab", REPOSITORY_NAME_OPTION_NAME)
	}
	repositoryOwner, ok := options[REPOSITORY_OWNER_OPTION_NAME]
	if !ok {
		log.Warnf("no repository owner passed to the '%s' service, some features may not work. Use the '%s' option to set this value", "GitLab", REPOSITORY_OWNER_OPTION_NAME)
	}

	log.Tracef("instantiating new GitLab service")

	client, err := newClientInstance(&uriString, &authenticationToken)
	if err != nil {
		return GitLab{}, &errs.NilPointerError{Message: fmt.Sprintf("could not create a GitLab service client"), Cause: err}
	}

	return newGitLab(client, &repositoryOwner, &repositoryName)
}

/*
Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the USERS feature.
*/
func (s GitLab) getAuthenticatedUser() (*GitLabUser, error) {
	log.Debugf("retrieving information for the authenticated user from the remote GitLab service")
	user, _, err := s.client.Users.CurrentUser()
	if err != nil {
		log.Debugf("an error occurred while retrieving information for the authenticated user from the remote GitLab service: %v", err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not retrieve the current GitLab user"), Cause: err}
	}
	log.Tracef("GitLab information for the authenticated user '%s' has been retrieved ", user.Username)
	res := newGitLabUser(*user)
	return res, nil
}

/*
Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the USERS feature.
*/
func (s GitLab) GetAuthenticatedUser() (*api.User, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	user, err := s.getAuthenticatedUser()
	if err != nil {
		return nil, err
	}
	var apiUser api.User = user
	return &apiUser, err
}

/*
Creates a new Git repository for the currently authenticated user.

Please note that if the service has been configured with repository owner and name those attributes are ignored
by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
name is always the name attribute.

Arguments are as follows:

- name the repository name. Cannot be nil
- description the repository description. It may be nil
- restricted when true the repository will have private visibility, otherwise it will be public
- initialize when true the repository is also initialized (usually with a default README file)

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the GIT_HOSTING feature.
*/
func (s GitLab) createGitRepository(name string, description *string, restricted bool, initialize bool) (*GitLabRepository, error) {
	log.Debugf("creating a new GitLab repository '%s'", name)
	options := &gl.CreateProjectOptions{Name: &name, InitializeWithReadme: &initialize}
	if description != nil {
		options.Description = description
	}
	var visibility gl.VisibilityValue
	if restricted {
		visibility = gl.PrivateVisibility
	} else {
		visibility = gl.PublicVisibility
	}
	options.Visibility = &visibility
	project, _, err := s.client.Projects.CreateProject(options)
	if err != nil {
		log.Debugf("an error occurred while creating the GitLab repository '%s': %v", name, err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not create the GitLab repository '%s'", name), Cause: err}
	}
	log.Tracef("GitLab repository '%s' has been created and is available at %s", name, project.WebURL)
	res := newGitLabRepository(*project)
	return res, nil
}

/*
Creates a new Git repository for the currently authenticated user.

Please note that if the service has been configured with repository owner and name those attributes are ignored
by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
name is always the name attribute.

Arguments are as follows:

- name the repository name. Cannot be nil
- description the repository description. It may be nil
- restricted when true the repository will have private visibility, otherwise it will be public
- initialize when true the repository is also initialized (usually with a default README file)

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the GIT_HOSTING feature.
*/
func (s GitLab) CreateGitRepository(name string, description *string, restricted bool, initialize bool) (*api.GitHostedRepository, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	repository, err := s.createGitRepository(name, description, restricted, initialize)
	if err != nil {
		return nil, err
	}
	var apiRepository api.GitHostedRepository = repository
	return &apiRepository, err
}

/*
Deletes a Git repository for the currently authenticated user.

Please note that if the service has been configured with repository owner and name those attributes are ignored
by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
name is always the name attribute.

Arguments are as follows:

- name the repository name. Cannot be nil

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the GIT_HOSTING feature.
*/
func (s GitLab) DeleteGitRepository(name string) error {
	log.Debugf("deleting the GitLab repository '%s'", name)
	// we need the owner, so let's fetch the current user ID
	user, err := s.getAuthenticatedUser()
	if err != nil {
		return err
	}
	_, err = s.client.Projects.DeleteProject(user.GetUserName() + "/" + name)
	if err != nil {
		log.Debugf("an error occurred while deleting the GitLab repository '%s': %v", name, err)
		return errs.TransportError{Message: fmt.Sprintf("could not delete the GitLab repository '%s'", name), Cause: err}
	}
	log.Tracef("GitLab repository '%s' has been deleted", name)
	return nil
}

/*
Returns a set of published assets for the release identified by the given attributes.

Arguments are as follows:

- owner the name of the owner of the repository of the release. It can't be nil
- repository the name of the repository of the release. It can't be nil
- tag the tag the release refers to (i.e. 1.2.3, v4.5.6). It can't be nil

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASE_ASSETS feature.
*/
func (s GitLab) listReleaseAssets(owner *string, repository *string, tag string) ([]ent.Attachment, error) {
	log.Debugf("retrieving information for GitLab release '%s' assets from the remote service", tag)
	requestOwner := ""
	if owner != nil {
		requestOwner = *owner
	} else if s.repositoryOwner != nil {
		requestOwner = *s.repositoryOwner
	} else {
		log.Warnf("the repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME)
	}
	requestRepository := ""
	if repository != nil {
		requestRepository = *repository
	} else if s.repositoryOwner != nil {
		requestRepository = *s.repositoryName
	} else {
		log.Warnf("the repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME)
	}

	assets, _, err := s.client.ReleaseLinks.ListReleaseLinks(requestOwner+"/"+requestRepository, tag, nil)
	if err != nil {
		log.Debugf("an error occurred while retrieving information for GitLab release '%s' assets from the remote service: %v", tag, err)
		return []ent.Attachment{}, errs.TransportError{Message: fmt.Sprintf("could not retrieve GitLab release assets"), Cause: err}
	}
	log.Tracef("GitLab release '%s' has %d assets", tag, len(assets))

	res := make([]ent.Attachment, len(assets))
	for i, asset := range assets {
		log.Tracef("GitLab release '%s' asset #%d is: %s - (%s) at %s", tag, i, asset.Name, string(asset.LinkType), asset.URL)
		emptyString := ""
		linkTypeString := string(asset.LinkType)
		res[i] = *ent.NewAttachmentWith(&asset.Name, &emptyString /* the description is not available by this API */, &asset.URL, &linkTypeString)
	}
	return res, nil
}

/*
Finds the release in the given repository by the release tag.

Arguments are as follows:

  - owner the name of the repository owner to get the release for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to get the release for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - tag the tag the release refers to (i.e. 1.2.3, v4.5.6). It can't be nil

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) getReleaseByTag(owner *string, repository *string, tag string) (*GitLabRelease, error) {
	log.Debugf("retrieving information for GitLab release '%s' from the remote service", tag)
	requestOwner := ""
	if owner != nil {
		requestOwner = *owner
	} else if s.repositoryOwner != nil {
		requestOwner = *s.repositoryOwner
	} else {
		log.Warnf("the repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME)
	}
	requestRepository := ""
	if repository != nil {
		requestRepository = *repository
	} else if s.repositoryOwner != nil {
		requestRepository = *s.repositoryName
	} else {
		log.Warnf("the repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME)
	}

	release, response, err := s.client.Releases.GetRelease(requestOwner+"/"+requestRepository, tag)
	if err != nil {
		if response.StatusCode == 404 {
			log.Debugf("no GitLab release with tag '%s' was found", tag)
			return nil, nil
		} else {
			log.Debugf("an error occurred while retrieving information for GitLab release '%s' from the remote service: %v", tag, err)
			return &GitLabRelease{}, errs.TransportError{Message: fmt.Sprintf("could not retrieve GitLab release by tag '%s'", tag), Cause: err}
		}
	}
	log.Tracef("information for GitLab release '%s' has been received from the remote service", tag)
	assets, err := s.listReleaseAssets(&requestOwner, &requestRepository, release.TagName)
	if err != nil {
		return &GitLabRelease{}, err
	}
	res := newGitLabRelease(*release).addAssets(assets)
	return res, nil
}

/*
Finds the release in the given repository by the release tag.

Arguments are as follows:

  - owner the name of the repository owner to get the release for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to get the release for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - tag the tag the release refers to (i.e. 1.2.3, v4.5.6). It can't be nil

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) GetReleaseByTag(owner *string, repository *string, tag string) (*api.Release, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	release, err := s.getReleaseByTag(owner, repository, tag)
	if err != nil {
		return nil, err
	}
	if release == nil {
		return nil, nil
	} else {
		var apiRelease api.Release = release
		return &apiRelease, err
	}
}

/*
Publishes a new release.

Arguments are as follows:

  - owner the name of the repository owner to create the release for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to create the release for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - title the release title, it may be the same of tag but not necessarily. It may be nil
  - tag tag to publish the release for (i.e. 1.2.3, v4.5.6). It can't be nil
  - description the release description. This is usually a Markdown text containing release notes or a changelog
    or something like that giving an overall description of the release
  - options the optional map of release options (RELEASE_OPTION_DRAFT, RELEASE_OPTION_PRE_RELEASE).
    When nil no options are evaluated.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) publishRelease(owner *string, repository *string, title *string, tag string, description *string, options *map[string]interface{}) (*GitLabRelease, error) {
	log.Debugf("publishing GitLab release '%s'", tag)
	requestOwner := ""
	if owner != nil {
		requestOwner = *owner
	} else if s.repositoryOwner != nil {
		requestOwner = *s.repositoryOwner
	} else {
		log.Warnf("the repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME)
	}
	requestRepository := ""
	if repository != nil {
		requestRepository = *repository
	} else if s.repositoryOwner != nil {
		requestRepository = *s.repositoryName
	} else {
		log.Warnf("the repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME)
	}

	if options != nil {
		_, ok := (*options)[api.RELEASE_OPTION_DRAFT]
		if ok {
			log.Debugf("the release options contain the '%s' option but GitLab does not support the flag. The option will be ignored.", api.RELEASE_OPTION_DRAFT)
		}
		_, ok = (*options)[api.RELEASE_OPTION_PRE_RELEASE]
		if ok {
			log.Debugf("the release options contain the '%s' option but GitLab does not support the flag. The option will be ignored.", api.RELEASE_OPTION_PRE_RELEASE)
		}
	}

	releaseOptions := &gl.CreateReleaseOptions{TagName: &tag}
	if title != nil {
		releaseOptions.Name = title
	}
	if description != nil {
		releaseOptions.Description = description
	}

	release, _, err := s.client.Releases.CreateRelease(requestOwner+"/"+requestRepository, releaseOptions)
	if err != nil {
		log.Debugf("an error occurred while publishing GitLab release '%s': %v", tag, err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not publish GitLab release with tag '%s'", tag), Cause: err}
	}
	log.Tracef("GitLab release '%s' has been published", tag)
	res := newGitLabRelease(*release)
	return res, nil
}

/*
Publishes a new release.

Arguments are as follows:

  - owner the name of the repository owner to create the release for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to create the release for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - title the release title, it may be the same of tag but not necessarily. It may be nil
  - tag tag to publish the release for (i.e. 1.2.3, v4.5.6). It can't be nil
  - description the release description. This is usually a Markdown text containing release notes or a changelog
    or something like that giving an overall description of the release
  - options the optional map of release options (RELEASE_OPTION_DRAFT, RELEASE_OPTION_PRE_RELEASE).
    When nil no options are evaluated.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) PublishRelease(owner *string, repository *string, title *string, tag string, description *string, options *map[string]interface{}) (*api.Release, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	release, err := s.publishRelease(owner, repository, title, tag, description, options)
	if err != nil {
		return nil, err
	}
	if release == nil {
		return nil, nil
	} else {
		var apiRelease api.Release = release
		return &apiRelease, err
	}
}

/*
Publishes a set of assets for a release. Even when the service supports the RELEASE_ASSETS
feature not all types of assets may be supported. Please check the implementation class for any restrictions
on the supported assets.

Returns the given release with also the links to the uploaded assets. The returned object represents the same
release provided as parameter but may be a different instance. Only the assets that were actually published
are returned while those not supported by the implementing service are not within the list of assets referred
by the returned object

Arguments are as follows:

  - owner the name of the repository owner to create the assets for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to create the assets for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - release the release to publish the assets for. It must be an object created by the same service
    implementation
  - assets the set of assets to publish. Assets may be interpreted differently depending on their
    path and type. Please check the implementation class for restrictions on the supported assets

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) publishReleaseAssets(owner *string, repository *string, release *GitLabRelease, assets []ent.Attachment) (*GitLabRelease, error) {
	if assets == nil || len(assets) == 0 {
		return release, nil
	}

	// The process here consists of 2 steps:
	// 1 - if the asset path is a local file, upload it to the package registry and get the returned URL from the API.
	//     If it's a URL skip this step and go to step 2
	// 2 - attach the link to the asset to the release

	log.Debugf("publishing %d assets for GitLab release '%s' to the remote service", len(assets), release.GetTag())
	requestOwner := ""
	if owner != nil {
		requestOwner = *owner
	} else if s.repositoryOwner != nil {
		requestOwner = *s.repositoryOwner
	} else {
		log.Warnf("the repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME)
	}
	requestRepository := ""
	if repository != nil {
		requestRepository = *repository
	} else if s.repositoryOwner != nil {
		requestRepository = *s.repositoryName
	} else {
		log.Warnf("the repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '%s' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME)
	}

	// step 1: upload local files to the package repository
	var result []ent.Attachment
	for _, asset := range assets {
		filePath := asset.GetPath()
		_, err := os.Stat(*filePath)
		if err == nil {
			file, err := os.Open(*filePath)
			log.Debugf("uploading asset '%s' (description: '%s', type: '%s', path: '%s')", *asset.GetFileName(), *asset.GetDescription(), *asset.GetType(), *asset.GetPath())
			// we always upload to the 'generic' package registry here
			genericPackageSelectValue := gl.SelectPackageFile
			publishedPackage, _, err := s.client.GenericPackages.PublishPackageFile(requestOwner+"/"+requestRepository, url.QueryEscape(*asset.GetDescription()), release.GetTag(), *asset.GetFileName(), file, &gl.PublishPackageFileOptions{Select: &genericPackageSelectValue})
			if err != nil {
				log.Debugf("an error occurred while uploading GitLab asset '%s': %v", *asset.GetFileName(), err)
				return nil, errs.TransportError{Message: fmt.Sprintf("an error occurred while uploading GitLab asset '%s'", *asset.GetFileName()), Cause: err}
			}
			assetURL := publishedPackage.File.URL
			log.Debugf("asset '%s' (type: '%s', path: '%s') has been uploaded and is available to URL '%s'", *asset.GetFileName(), *asset.GetType(), *asset.GetPath(), assetURL)
			result = append(result, *ent.NewAttachmentWith(asset.GetFileName(), asset.GetDescription(), &assetURL, asset.GetType()))
		} else {
			log.Debugf("the path '%s' for the asset '%s' cannot be resolved to a local file", *asset.GetPath(), *asset.GetFileName())

			// just check if it's a valid URL and if it is add it to the result assets
			_, err := url.ParseRequestURI(*asset.GetPath())
			if err == nil {
				result = append(result, asset)
			} else {
				log.Warnf("the path '%s' for the asset '%s' cannot be resolved to a local file and is not a valid URL and will be skipped", *asset.GetPath(), *asset.GetFileName())
			}
		}
	}

	// step 2: upload asset links to the release
	for _, asset := range result {
		log.Debugf("updating release '%s' with asset '%s' (description: '%s', type: '%s', path: '%s')", release.GetTag(), *asset.GetFileName(), *asset.GetDescription(), *asset.GetType(), *asset.GetPath())

		_, _, err := s.client.ReleaseLinks.CreateReleaseLink(requestOwner+"/"+requestRepository, release.GetTag(), &gl.CreateReleaseLinkOptions{Name: /*asset.GetName()*/ asset.GetDescription(), URL: asset.GetPath()})
		if err != nil {
			log.Debugf("an error occurred while updating GitLab asset '%s': %v", *asset.GetFileName(), err)
			return nil, errs.TransportError{Message: fmt.Sprintf("could not update GitLab asset '%s'", *asset.GetFileName()), Cause: err}
		}
	}

	log.Debugf("uploaded %d assets", len(result))

	return &GitLabRelease{title: release.GetTitle(), tag: release.GetTag(), assets: result}, nil
}

/*
Publishes a set of assets for a release. Even when the service supports the RELEASE_ASSETS
feature not all types of assets may be supported. Please check the implementation class for any restrictions
on the supported assets.

Returns the given release with also the links to the uploaded assets. The returned object represents the same
release provided as parameter but may be a different instance. Only the assets that were actually published
are returned while those not supported by the implementing service are not within the list of assets referred
by the returned object

Arguments are as follows:

  - owner the name of the repository owner to create the assets for. It may be nil, in which case,
    the repository owner must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - repository the name of the repository to create the assets for. It may be nil, in which case,
    the repository name must be passed as a service option (see services implementing this interface for more
    details on the options they accept). If not nil this value overrides the option passed to the service.
  - release the release to publish the assets for. It must be an object created by the same service
    implementation
  - assets the set of assets to publish. Assets may be interpreted differently depending on their
    path and type. Please check the implementation class for restrictions on the supported assets

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitLab) PublishReleaseAssets(owner *string, repository *string, release *api.Release, assets []ent.Attachment) (*api.Release, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	inputRelease, castOK := (*release).(*GitLabRelease)
	if !castOK {
		return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the given release must be of type GitLabRelease")}
	}

	outputRelease, err := s.publishReleaseAssets(owner, repository, inputRelease, assets)
	if err != nil {
		return nil, err
	}
	if outputRelease == nil {
		return nil, nil
	} else {
		var apiRelease api.Release = outputRelease
		return &apiRelease, err
	}
}

/*
Safely checks if the underlying implementation supports the given operation. If this
method returns true then the underlying class will not raise any
UnsupportedOperationError when invoking the specific methods.

Arguments are as follows:

- feature the feature to check for support.
*/
func (s GitLab) Supports(feature api.Feature) bool {
	switch feature {
	case api.GIT_HOSTING:
		return true
	case api.RELEASES:
		return true
	case api.RELEASE_ASSETS:
		return true
	case api.USERS:
		return true
	default:
		return false
	}
}
