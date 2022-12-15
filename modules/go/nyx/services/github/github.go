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

package github

import (
	"context"  // https://pkg.go.dev/context
	"fmt"      // https://pkg.go.dev/fmt
	"net/http" // https://pkg.go.dev/net/http
	"os"       // https://pkg.go.dev/os
	"strings"  // https://pkg.go.dev/strings

	gh "github.com/google/go-github/github" // https://pkg.go.dev/github.com/google/go-github/github
	log "github.com/sirupsen/logrus"        // https://github.com/Sirupsen/logrus, https://pkg.go.dev/github.com/sirupsen/logrus
	oauth2 "golang.org/x/oauth2"            // https://pkg.go.dev/golang.org/x/oauth2

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
		If the repository is https://github.com/octocat/hello-world, the value to pass for
		this option is 'hello-world'.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to perform some of its operations.
	*/
	REPOSITORY_NAME_OPTION_NAME = "REPOSITORY_NAME"

	/*
		The name of the option used to pass the owner of the Git repository to this object instance.
		If the repository is https://github.com/octocat/hello-world, the value to pass for
		this option is 'octocat'. This option accepts individual and organization names.
		This is the value of the key inside the options passed to get a new instance of this class.
		If this option is not passed the service will not be able to perform some of its operations.
	*/
	REPOSITORY_OWNER_OPTION_NAME = "REPOSITORY_OWNER"
)

/*
The entry point to the GitHub remote service.
*/
type GitHub struct {
	// The name of the repository owner, used when using APIs that require the name of the repository
	// owner (individual or organization). It may be nil, but some operations may fail.
	repositoryOwner *string

	// The name of the repository, used when using APIs that require the name of the repository.
	// It may be nil, but some operations may fail.
	repositoryName *string

	// The private API client instance.
	client gh.Client
}

/*
Builds an instance using the given API.

Arguments are as follows:

  - client the API client to be used internally
  - repositoryOwner the name of the repository owner, used when using APIs that require the
    name of the repository owner (individual or organization). It may be nil, but some operations may fail
  - repositoryName the name of the repository, used when using APIs that require the
    name of the repository. It may be nil, but some operations may fail

Errors can be:

- NilPointerError if the given API is nil
*/
func newGitHub(client gh.Client, repositoryOwner *string, repositoryName *string) (GitHub, error) {
	res := GitHub{}
	res.client = client
	res.repositoryOwner = repositoryOwner
	res.repositoryName = repositoryName
	return res, nil
}

/*
Returns a new GitHub client instance.

Arguments are as follows:

- baseURI the custom endpoint to use (for private GitHub instances). If nil or empty the standard endpoint will be used
- authenticationToken the authentication token to use. If nil or empty no authentication is used

Errors can be returned by the underlying implementation
*/
func newClientInstance(baseURI *string, authenticationToken *string) (gh.Client, error) {
	log.Tracef("instantiating new GitHub client")
	var httpClient *http.Client = nil
	if authenticationToken != nil && "" != strings.TrimSpace(*authenticationToken) {
		log.Debugf("the new GitHub service will use the given authentication token")
		ctx := context.Background()

		tokenSource := oauth2.StaticTokenSource(
			&oauth2.Token{AccessToken: *authenticationToken},
		)
		httpClient = oauth2.NewClient(ctx, tokenSource)
	} else {
		log.Debugf("the new GitHub service does not use authentication because no token was passed")
	}

	if baseURI != nil && "" != strings.TrimSpace(*baseURI) {
		log.Tracef("the new GitHub service uses the custom URI '%s'", *baseURI)
		client, err := gh.NewEnterpriseClient(*baseURI, *baseURI, httpClient)
		return *client, err

	} else {
		log.Tracef("the new GitHub service uses the default URI")
		client := gh.NewClient(httpClient)
		return *client, nil
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
func Instance(options map[string]string) (GitHub, error) {
	if options == nil {
		return GitHub{}, &errs.NilPointerError{Message: fmt.Sprintf("can't create a new instance with a null options map")}
	}

	uriString, ok := options[BASE_URI_OPTION_NAME]
	if !ok {
		log.Debugf("no custom URI passed to the '%s' service, the default endpoint will be used", "GitHub")
	}
	authenticationToken, ok := options[AUTHENTICATION_TOKEN_OPTION_NAME]
	if !ok {
		log.Warnf("no authentication token passed to the '%s' service, no authentication protected operation will be available. Use the '%s' option to set this value", "GitHub", AUTHENTICATION_TOKEN_OPTION_NAME)
	}
	repositoryName, ok := options[REPOSITORY_NAME_OPTION_NAME]
	if !ok {
		log.Warnf("no repository name passed to the '%s' service, some features may not work. Use the '%s' option to set this value", "GitHub", REPOSITORY_NAME_OPTION_NAME)
	}
	repositoryOwner, ok := options[REPOSITORY_OWNER_OPTION_NAME]
	if !ok {
		log.Warnf("no repository owner passed to the '%s' service, some features may not work. Use the '%s' option to set this value", "GitHub", REPOSITORY_OWNER_OPTION_NAME)
	}

	log.Tracef("instantiating new GitHub service")

	client, err := newClientInstance(&uriString, &authenticationToken)
	if err != nil {
		return GitHub{}, &errs.NilPointerError{Message: fmt.Sprintf("could not create a GitHub service client"), Cause: err}
	}

	return newGitHub(client, &repositoryOwner, &repositoryName)
}

/*
Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the USERS feature.
*/
func (s GitHub) getAuthenticatedUser() (*GitHubUser, error) {
	log.Debugf("retrieving information for the authenticated user from the remote GitHub service")
	// passing an empty user name retrieves the authenticated user
	user, _, err := s.client.Users.Get(context.Background(), "")
	if err != nil {
		log.Debugf("an error occurred while retrieving information for the authenticated user from the remote GitHub service: %v", err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not retrieve the current GitHub user"), Cause: err}
	}
	log.Tracef("GitHub information for the authenticated user '%s' has been retrieved ", *user.Login)
	res := newGitHubUser(*user)
	return res, nil
}

/*
Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the USERS feature.
*/
func (s GitHub) GetAuthenticatedUser() (*api.User, error) {
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
func (s GitHub) createGitRepository(name string, description *string, restricted bool, initialize bool) (*GitHubRepository, error) {
	log.Debugf("creating a new GitHub repository '%s'", name)
	repository := &gh.Repository{Name: &name, Private: &restricted, AutoInit: &initialize}
	if description != nil {
		repository.Description = description
	}
	// passing an empty organization creates the repository for the authenticated user
	repository, _, err := s.client.Repositories.Create(context.Background(), "", repository)
	if err != nil {
		log.Debugf("an error occurred while creating the GitHub repository '%s': %v", name, err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not create the GitHub repository '%s'", name), Cause: err}
	}
	log.Tracef("GitHub repository '%s' has been created and is available at %s", name, *repository.HTMLURL)
	res := newGitHubRepository(*repository)
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
func (s GitHub) CreateGitRepository(name string, description *string, restricted bool, initialize bool) (*api.GitHostedRepository, error) {
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
func (s GitHub) DeleteGitRepository(name string) error {
	log.Debugf("deleting the GitHub repository '%s'", name)
	// we need the owner, so let's fetch the current user ID
	user, err := s.getAuthenticatedUser()
	if err != nil {
		return err
	}
	_, err = s.client.Repositories.Delete(context.Background(), user.GetUserName(), name)
	if err != nil {
		log.Debugf("an error occurred while deleting the GitHub repository '%s': %v", name, err)
		return errs.TransportError{Message: fmt.Sprintf("could not delete the GitHub repository '%s'", name), Cause: err}
	}
	log.Tracef("GitHub repository '%s' has been deleted", name)
	return nil
}

/*
Returns a set of published assets for the release identified by the given attributes.

Arguments are as follows:

- owner the name of the owner of the repository of the release. It can't be nil
- repository the name of the repository of the release. It can't be nil
- id the release ID. It can't be nil

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASE_ASSETS feature.
*/
func (s GitHub) listReleaseAssets(owner *string, repository *string, id int64) ([]ent.Attachment, error) {
	log.Debugf("retrieving information for GitHub release '%d' assets from the remote service", id)
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

	assets, _, err := s.client.Repositories.ListReleaseAssets(context.Background(), requestOwner, requestRepository, id, nil)
	if err != nil {
		log.Debugf("an error occurred while retrieving information for GitHub release '%d' assets from the remote service: %v", id, err)
		return []ent.Attachment{}, errs.TransportError{Message: fmt.Sprintf("could not retrieve GitHub release assets"), Cause: err}
	}
	log.Tracef("GitHub release '%d' has %d assets", id, len(assets))

	res := make([]ent.Attachment, len(assets))
	for i, asset := range assets {
		log.Tracef("GitHub release '%d' asset #%d is: %s - %s (%s) at %s", id, i, *asset.Name, *asset.Label, *asset.ContentType, *asset.URL)
		// the Label (used for the asset Description) is not available for assets we have uploaded ourselves because the API does not support it when uploading (see the publishReleaseAssets method)
		res[i] = *ent.NewAttachmentWith(asset.Name, asset.Label, asset.URL, asset.ContentType)
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
func (s GitHub) getReleaseByTag(owner *string, repository *string, tag string) (*GitHubRelease, error) {
	log.Debugf("retrieving information for GitHub release '%s' from the remote service", tag)
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

	release, response, err := s.client.Repositories.GetReleaseByTag(context.Background(), requestOwner, requestRepository, tag)
	if err != nil {
		if response.StatusCode == 404 {
			log.Debugf("no GitHub release with tag '%s' was found", tag)
			return nil, nil
		} else {
			log.Debugf("an error occurred while retrieving information for GitHub release '%s' from the remote service: %v", tag, err)
			return &GitHubRelease{}, errs.TransportError{Message: fmt.Sprintf("could not retrieve GitHub release by tag '%s'", tag), Cause: err}
		}
	}
	log.Tracef("information for GitHub release '%s' has been received from the remote service", tag)
	assets, err := s.listReleaseAssets(&requestOwner, &requestRepository, *release.ID)
	if err != nil {
		return &GitHubRelease{}, err
	}
	res := newGitHubRelease(*release).addAssets(assets)
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
func (s GitHub) GetReleaseByTag(owner *string, repository *string, tag string) (*api.Release, error) {
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

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitHub) publishRelease(owner *string, repository *string, title *string, tag string, description *string) (*GitHubRelease, error) {
	log.Debugf("publishing GitHub release '%s'", tag)
	release := &gh.RepositoryRelease{TagName: &tag}
	if title != nil {
		release.Name = title
	}
	if description != nil {
		release.Body = description
	}

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

	release, _, err := s.client.Repositories.CreateRelease(context.Background(), requestOwner, requestRepository, release)
	if err != nil {
		log.Debugf("an error occurred while publishing GitHub release '%s': %v", tag, err)
		return nil, errs.TransportError{Message: fmt.Sprintf("could not publish GitHub release with tag '%s'", tag), Cause: err}
	}
	log.Tracef("GitHub release '%s' has been published", tag)
	res := newGitHubRelease(*release)
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

Errors can be:

- SecurityError if authentication or authorization fails or there is no currently authenticated user
- TransportError if communication to the remote endpoint fails
- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
*/
func (s GitHub) PublishRelease(owner *string, repository *string, title *string, tag string, description *string) (*api.Release, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	release, err := s.publishRelease(owner, repository, title, tag, description)
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
func (s GitHub) publishReleaseAssets(owner *string, repository *string, release *GitHubRelease, assets []ent.Attachment) (*GitHubRelease, error) {
	log.Debugf("publishing %d assets for GitHub release '%s' to the remote service", len(assets), release.GetTag())
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

	for i, asset := range assets {
		log.Debugf("publishing asset %d out of %d for GitHub release '%s' to the remote service (%s - %s (%s))", i, len(assets), release.GetTag(), *asset.GetPath(), *asset.GetDescription(), *asset.GetType())
		filePath := asset.GetPath()
		_, err := os.Stat(*filePath)
		if err == nil {
			file, err := os.Open(*filePath)
			if err != nil {
				log.Debugf("an error occurred while opening file %s for asset %d out of %d: %v", *asset.GetPath(), i, len(assets), err)
				return nil, errs.TransportError{Message: fmt.Sprintf("could not open file '%s'", *asset.GetPath()), Cause: err}
			}
			// This API does not support passing the Label (used for the Description), which is actually supported by the REST API but not by this library. Only the file name can be used here
			releaseAsset, _, err := s.client.Repositories.UploadReleaseAsset(context.Background(), requestOwner, requestRepository, release.GetID(), &gh.UploadOptions{Name: *asset.GetFileName()}, file)
			if err != nil {
				log.Debugf("an error occurred while publishing file %s for asset %d out of %d to the remote GitHub service: %v", *asset.GetPath(), i, len(assets), err)
				return nil, errs.TransportError{Message: fmt.Sprintf("could not upload release asset '%s'", *asset.GetPath()), Cause: err}
			}
			log.Debugf("asset %d out of %d for GitHub release '%s' has been published to the remote service (%s - %s (%s): %s)", i, len(assets), release.GetTag(), *asset.GetFileName(), *asset.GetDescription(), *asset.GetType(), *releaseAsset.URL)
			release.addAsset(*ent.NewAttachmentWith(asset.GetFileName(), asset.GetDescription(), releaseAsset.URL, asset.GetType()))
		} else {
			log.Warnf("the path '%s' for the asset '%s' cannot be resolved to a local file and will be skipped", *asset.GetPath(), *asset.GetFileName())
		}
	}
	return release, nil
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
func (s GitHub) PublishReleaseAssets(owner *string, repository *string, release *api.Release, assets []ent.Attachment) (*api.Release, error) {
	// This is the method exposed to the outside and the following just casts values to/from the internal implementation
	inputRelease, castOK := (*release).(*GitHubRelease)
	if !castOK {
		return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the given release must be of type GitHubRelease")}
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
func (s GitHub) Supports(feature api.Feature) bool {
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
