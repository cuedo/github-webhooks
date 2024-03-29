-- | Smoke tests to make sure that the Aeson FromJSON instances work as expected.
module DecodeEventsSpec
    ( spec
    ) where

import           Test.Hspec
import           Control.Monad               ( void )
import           Data.Aeson                  ( FromJSON, eitherDecode' )
import qualified Data.Text                   as T
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Vector                 as V

import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload


-- Run GHCi:
-- $ stack install pretty-show
-- $ stack ghci --test pretty-show

-- Paste:
{-
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import GitHub.Data.Webhooks.Events
import Text.Show.Pretty
import Text.PrettyPrint.HughesPJ
-}

-- Use:
-- do { x <- BSL.readFile "fixtures/watch-event.json";
--      putStrLn $ renderStyle (style {lineLength=200}) . ppDoc $ either error id (eitherDecode' x :: Either String WatchEvent) }

fixtureShouldMatch :: (FromJSON a, Show a, Eq a) => String -> a -> IO ()
fixtureShouldMatch path expected = do
  str <- BSL.readFile path
  let output = eitherDecode' str
  void $ output `shouldBe` Right expected

spec :: Spec
spec = do
  it "can decode CheckSuiteEvent"         $ fixtureShouldMatch "fixtures/check-suite-event.json" checkSuiteEventFixture
  it "can decode CheckRunEvent"           $ fixtureShouldMatch "fixtures/check-run-event.json" checkRunEventFixture
  it "can decode CommitCommentEvent"      $ fixtureShouldMatch "fixtures/commit-comment-event.json" commitCommentEventFixture
  it "can decode CreateEvent"             $ fixtureShouldMatch "fixtures/create-event.json" createEventFixture
  it "can decode DeleteEvent"             $ fixtureShouldMatch "fixtures/delete-event.json" deleteEventFixture
  it "can decode DeploymentEvent"         $ fixtureShouldMatch "fixtures/deployment-event.json" deploymentEventFixture
  it "can decode DeploymentStatusEvent"   $ fixtureShouldMatch "fixtures/deployment-status-event.json" deploymentStatusEventFixture
  it "can decode ForkEvent"               $ fixtureShouldMatch "fixtures/fork-event.json" forkEventFixture
  it "can decode GollumEvent"             $ fixtureShouldMatch "fixtures/gollum-event.json" gollumEventFixture
  it "can decode InstallationEvent"       $ fixtureShouldMatch "fixtures/installation-event.json" installationEventFixture
  it "can decode InstallationRepositoriesEvent" $ fixtureShouldMatch "fixtures/installation-repo-event.json" installationRepoEventFixture
  it "can decode IssueCommentEvent"       $ fixtureShouldMatch "fixtures/issue-comment-event.json" issueCommentEventFixture
  it "can decode IssuesEvent"             $ fixtureShouldMatch "fixtures/issues-event.json" issuesEventFixture
  it "can decode LabelEvent"              $ fixtureShouldMatch "fixtures/label-event.json" labelEventFixture
  it "can decode MarketplacePurchaseEvent" $ fixtureShouldMatch "fixtures/marketplace-purchase-event.json" marketplacePurchaseEventFixture
  it "can decode MemberEvent"             $ fixtureShouldMatch "fixtures/member-event.json" memberEventFixture
  it "can decode MembershipEvent"         $ fixtureShouldMatch "fixtures/membership-event.json" membershipEventFixture
  it "can decode MilestoneEvent"          $ fixtureShouldMatch "fixtures/milestone-event.json" milestoneEventFixture
  it "can decode OrgBlockEvent"           $ fixtureShouldMatch "fixtures/org-block-event.json" orgBlockEventFixture
  it "can decode OrganizationEvent"       $ fixtureShouldMatch "fixtures/organization-event.json" organizationEventFixture
  it "can decode PageBuildEvent"          $ fixtureShouldMatch "fixtures/page-build-event.json" pageBuildEventFixture
  it "can decode ProjectCardEvent"        $ fixtureShouldMatch "fixtures/project-card-event.json" projectCardEventFixture
  it "can decode ProjectColumnEvent"      $ fixtureShouldMatch "fixtures/project-column-event.json" projectColumnEventFixture
  it "can decode ProjectEvent"            $ fixtureShouldMatch "fixtures/project-event.json" projectEventFixture
  it "can decode PublicEvent"             $ fixtureShouldMatch "fixtures/public-event.json" publicEventFixture
  it "can decode PullRequestEvent"        $ fixtureShouldMatch "fixtures/pull-request-event.json" pullRequestEventFixture
  it "can decode PullRequestEvent when body is null" $ fixtureShouldMatch "fixtures/pull-request-event-null-body.json" pullRequestEventNullBodyFixture
  it "can decode PullRequestEvent when head.repo is null" $ fixtureShouldMatch "fixtures/pull-request-delete-null-head-repo-anomaly.json" pullRequestEventDeleteNullHeadRepoAnomalyFixture
  it "can decode PullRequestReviewCommentEvent" $ fixtureShouldMatch  "fixtures/pull-request-review-comment-event.json" pullRequestReviewCommentEventFixture
  it "can decode PullRequestReviewEvent"  $ fixtureShouldMatch "fixtures/pull-request-review-event.json" pullRequestReviewEventFixture
  it "can decode PushEvent"               $ fixtureShouldMatch "fixtures/push-event.json" pushEventFixture
  it "can decode PushEvent without sender" $ fixtureShouldMatch "fixtures/push-event-without-sender.json" pushEventFixtureWithoutSender
  it "can decode ReleaseEvent"            $ fixtureShouldMatch "fixtures/release-event.json" releaseEventFixture
  it "can decode RepositoryEvent"         $ fixtureShouldMatch "fixtures/repository-event.json" repositoryEventFixture
  it "can decode StatusEvent"             $ fixtureShouldMatch "fixtures/status-event.json" statusEventFixture
  it "can decode TeamAddEvent"            $ fixtureShouldMatch "fixtures/team-add-event.json" teamAddEventFixture
  it "can decode TeamEvent"               $ fixtureShouldMatch "fixtures/team-event.json" teamEventFixture
  it "can decode WatchEvent"              $ fixtureShouldMatch "fixtures/watch-event.json" watchEventFixture


-- Fixtures
-- These correspond to the expected decoded output of the above JSON files.

checkSuiteEventFixture :: CheckSuiteEvent
checkSuiteEventFixture = CheckSuiteEvent
    { evCheckSuiteAction = CheckSuiteEventActionCompleted
    , evCheckSuiteCheckSuite =
        HookCheckSuite
          { whCheckSuiteId = 12341234111
          , whCheckSuiteNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whCheckSuiteHeadBranch = Just "test-pr"
          , whCheckSuiteHeadSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
          , whCheckSuiteStatus = HookCheckSuiteStatusCompleted
          , whCheckSuiteConclusion = Just HookCheckSuiteConclusionActionRequired
          , whCheckSuiteUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/check-suites/123451234"
          , whCheckSuiteBeforeSha = Just "15c99c3e0b9d840d8465be47813cf39686815f2e"
          , whCheckSuiteAfterSha = Just "45deaf5013c757e58e2665849c3fd3add3edfa59"
          , whCheckSuitePullRequests =
              V.fromList [
                HookChecksPullRequest
                  { whChecksPullRequestUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
                  , whChecksPullRequestId = 12345123222
                  , whChecksPullRequestNumber = 1
                  , whChecksPullRequestHead =
                      HookChecksPullRequestTarget
                        { whChecksPullRequestTargetSha = "a68c473cf629b651f6e615c7d0eea95811d2db3d"
                        , whChecksPullRequestTargetRef = "test-pr"
                        , whChecksPullRequestTargetRepo =
                            HookChecksPullRequestRepository
                              { whChecksPullRequestRepositoryId = 12345123333
                              , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                              , whChecksPullRequestRepositoryName = "public-repo"
                              }
                        }
                  , whChecksPullRequestBase =
                      HookChecksPullRequestTarget
                        { whChecksPullRequestTargetSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
                        , whChecksPullRequestTargetRef = "master"
                        , whChecksPullRequestTargetRepo =
                            HookChecksPullRequestRepository
                              { whChecksPullRequestRepositoryId = 123451234444
                              , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                              , whChecksPullRequestRepositoryName = "public-repo"
                              }
                        }
                  }
              ]
          , whCheckSuiteCreatedAt = read "2020-02-18 00:54:04Z"
          , whCheckSuiteUpdatedAt = read "2020-02-18 00:54:04Z"
          , whCheckSuiteLatestCheckRunsCount = Just 0
          , whCheckSuiteCheckRunsUrl = Just (URL "https://api.github.com/repos/baxterthehacker/public-repo/check-suites/12341234111/check-runs")
          , whCheckSuiteHeadCommit =
              Just
                HookCheckSuiteCommit
                  { whCheckSuiteCommitSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
                  , whCheckSuiteCommitAuthor =
                      HookSimpleUser
                        { whSimplUserName = "baxterthehacker"
                        , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                        , whSimplUserLogin = Nothing
                        }
                  , whCheckSuiteCommitCommitter =
                      HookSimpleUser
                        { whSimplUserName = "baxterthehacker"
                        , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                        , whSimplUserLogin = Nothing
                        }
                  }
          }
    , evCheckSuiteRepository =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evCheckSuiteOrganization =
        Just
          HookOrganization
            { whOrgLogin = "baxterandthehackers"
            , whOrgId = 4312013
            , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
            , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
            , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
            , whOrgHooksUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/hooks")
            , whOrgIssuesUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/issues")
            , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
            , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
            , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
            , whOrgDescription = T.empty
            }
    , evCheckSuiteSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evCheckSuiteInstallation =
        Just
          HookChecksInstallation
            { whChecksInstallationId = 1234123
            , whChecksInstallationNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            }
    }

checkRunEventFixture :: CheckRunEvent
checkRunEventFixture = CheckRunEvent
    { evCheckRunAction = CheckRunEventActionRequestedAction
    , evCheckRunCheckRun =
        HookCheckRun
          { whCheckRunId = 123412411
          , whCheckRunNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whCheckRunHeadSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
          , whCheckRunExternalId = ""
          , whCheckRunUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/check-runs/454339238"
          , whCheckRunHtmlUrl =  URL"https://github.com/baxterthehacker/public-repo/runs/454339238"
          , whCheckRunDetailsUrl = URL "https://www.baxterthehacker.com"
          , whCheckRunStatus = HookCheckRunStatusCompleted
          , whCheckRunConclusion = Just HookCheckRunConclusionStale
          , whCheckRunStartedAt = read "2020-02-19 02:44:54Z"
          , whCheckRunCompletedAt = Nothing
          , whCheckRunOutput =
              HookCheckRunOutput
                { whCheckRunOutputTitle = Nothing
                , whCheckRunOutputSummary = Nothing
                , whCheckRunOutputText = Nothing
                , whCheckRunOutputAnnotationsCount = 0
                , whCheckRunOutputAnnotationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/check-runs/454339238/annotations"
                }
          , whCheckRunName = "Test Check Run"
          , whCheckRunCheckSuite =
              HookCheckSuite
                { whCheckSuiteId = 12341234111
                , whCheckSuiteNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whCheckSuiteHeadBranch = Just "test-pr"
                , whCheckSuiteHeadSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
                , whCheckSuiteStatus = HookCheckSuiteStatusQueued
                , whCheckSuiteConclusion = Nothing
                , whCheckSuiteUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/check-suites/123451234"
                , whCheckSuiteBeforeSha = Just "15c99c3e0b9d840d8465be47813cf39686815f2e"
                , whCheckSuiteAfterSha = Just "45deaf5013c757e58e2665849c3fd3add3edfa59"
                , whCheckSuitePullRequests =
                    V.fromList [
                      HookChecksPullRequest
                        { whChecksPullRequestUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
                        , whChecksPullRequestId = 12345123222
                        , whChecksPullRequestNumber = 1
                        , whChecksPullRequestHead =
                            HookChecksPullRequestTarget
                              { whChecksPullRequestTargetSha = "a68c473cf629b651f6e615c7d0eea95811d2db3d"
                              , whChecksPullRequestTargetRef = "test-pr"
                              , whChecksPullRequestTargetRepo =
                                  HookChecksPullRequestRepository
                                    { whChecksPullRequestRepositoryId = 12345123333
                                    , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                                    , whChecksPullRequestRepositoryName = "public-repo"
                                    }
                              }
                        , whChecksPullRequestBase =
                            HookChecksPullRequestTarget
                              { whChecksPullRequestTargetSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
                              , whChecksPullRequestTargetRef = "master"
                              , whChecksPullRequestTargetRepo =
                                  HookChecksPullRequestRepository
                                    { whChecksPullRequestRepositoryId = 123451234444
                                    , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                                    , whChecksPullRequestRepositoryName = "public-repo"
                                    }
                              }
                        }
                    ]
                , whCheckSuiteCreatedAt = read "2020-02-18 00:54:04Z"
                , whCheckSuiteUpdatedAt = read "2020-02-18 00:54:04Z"
                , whCheckSuiteLatestCheckRunsCount = Nothing
                , whCheckSuiteCheckRunsUrl = Nothing
                , whCheckSuiteHeadCommit = Nothing
                }
          , whCheckRunPullRequests =
              V.fromList [
                HookChecksPullRequest
                  { whChecksPullRequestUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
                  , whChecksPullRequestId = 12345123222
                  , whChecksPullRequestNumber = 1
                  , whChecksPullRequestHead =
                      HookChecksPullRequestTarget
                        { whChecksPullRequestTargetSha = "a68c473cf629b651f6e615c7d0eea95811d2db3d"
                        , whChecksPullRequestTargetRef = "test-pr"
                        , whChecksPullRequestTargetRepo =
                            HookChecksPullRequestRepository
                              { whChecksPullRequestRepositoryId = 12345123333
                              , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                              , whChecksPullRequestRepositoryName = "public-repo"
                              }
                        }
                  , whChecksPullRequestBase =
                      HookChecksPullRequestTarget
                        { whChecksPullRequestTargetSha = "45deaf5013c757e58e2665849c3fd3add3edfa59"
                        , whChecksPullRequestTargetRef = "master"
                        , whChecksPullRequestTargetRepo =
                            HookChecksPullRequestRepository
                              { whChecksPullRequestRepositoryId = 123451234444
                              , whChecksPullRequestRepositoryUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                              , whChecksPullRequestRepositoryName = "public-repo"
                              }
                        }
                  }
              ]
          }
    , evCheckRunRequestedAction =
        Just
          HookCheckRunRequestedAction
            { whCheckRunRequestedActionIdentifier = "fix_errors"
            }
    , evCheckRunRepository =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evCheckRunOrganization =
        Just
          HookOrganization
            { whOrgLogin = "baxterandthehackers"
            , whOrgId = 4312013
            , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
            , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
            , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
            , whOrgHooksUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/hooks")
            , whOrgIssuesUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/issues")
            , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
            , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
            , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
            , whOrgDescription = T.empty
            }
    , evCheckRunSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evCheckRunInstallation =
        Just
          HookChecksInstallation
            { whChecksInstallationId = 1234123
            , whChecksInstallationNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            }
    }

commitCommentEventFixture :: CommitCommentEvent
commitCommentEventFixture = CommitCommentEvent
    { evCommitCommentAction = CommitCommentActionCreated
    , evCommitCommentPayload =
        HookCommitComment
          { whCommitCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments/11056394"
          , whCommitCommentHtmlUrl =
              URL "https://github.com/baxterthehacker/public-repo/commit/9049f1265b7d61be4a8904a9a27120d2064dab3b#commitcomment-11056394"
          , whCommitCommentId = 11056394
          , whCommitCommentNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whCommitCommentUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whCommitCommentPos = Nothing
          , whCommitCommentLine = Nothing
          , whCommitCommentPath = Nothing
          , whCommitCommentCommitSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whCommitCommentCreatedAt = read "2015-05-05 23:40:29Z"
          , whCommitCommentUpdatedAt = read "2015-05-05 23:40:29Z"
          , whCommitCommentBody = "This is a really good change! :+1:"
          }
    , evCommitCommentRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evCommitCommentSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

createEventFixture :: CreateEvent
createEventFixture = CreateEvent
    { evCreateRef = "0.0.1"
    , evCreateRefType = "tag"
    , evCreateMasterBranch = "master"
    , evCreateDescription = ""
    , evCreatePusherType = OwnerUser
    , evCreateRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:38Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evCreateSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

deleteEventFixture :: DeleteEvent
deleteEventFixture = DeleteEvent
    { evDeleteRef = "simple-tag"
    , evDeleteRefType = "tag"
    , evDeletePusherType = OwnerUser
    , evDeleteRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:40Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evDeleteSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

deploymentEventFixture :: DeploymentEvent
deploymentEventFixture = DeploymentEvent
    { evDeploymentInfo =
        HookDeployment
          { whDeploymentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692"
          , whDeploymentId = 710692
          , whDeploymentNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whDeploymentSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whDeploymentRef = "master"
          , whDeploymentTask = "deploy"
          , whDeploymentEnv = "production"
          , whDeploymentDescription = Nothing
          , whDeploymentCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whDeploymentCreatedAt = read "2015-05-05 23:40:38Z"
          , whDeploymentUpdatedAt = read "2015-05-05 23:40:38Z"
          , whDeploymentStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692/statuses"
          , whDeploymentRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          }
    , evDeploymentRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:38Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evDeploymentSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

deploymentStatusEventFixture :: DeploymentStatusEvent
deploymentStatusEventFixture = DeploymentStatusEvent
    { evDeplStatusInfo =
        HookDeploymentStatus
          { whDeploymentStatusUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692/statuses/1115122"
          , whDeploymentStatusId = 1115122
          , whDeploymentStatusNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whDeploymentStatusState = "success"
          , whDeploymentStatusCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whDeploymentStatusDesc = Nothing
          , whDeploymentStatusTargetUrl = Nothing
          , whDeploymentStatusCreatedAt = read "2015-05-05 23:40:39Z"
          , whDeploymentStatusUpdatedAt = read "2015-05-05 23:40:39Z"
          , whDeploymentStatusDeplUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692"
          , whDeploymentStatusRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          }
    , evDeplStatusDeployment =
        HookDeployment
          { whDeploymentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692"
          , whDeploymentId = 710692
          , whDeploymentNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whDeploymentSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whDeploymentRef = "master"
          , whDeploymentTask = "deploy"
          , whDeploymentEnv = "production"
          , whDeploymentDescription = Nothing
          , whDeploymentCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whDeploymentCreatedAt = read "2015-05-05 23:40:38Z"
          , whDeploymentUpdatedAt = read "2015-05-05 23:40:38Z"
          , whDeploymentStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/deployments/710692/statuses"
          , whDeploymentRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          }
    , evDeplStatusRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:38Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evDeplStatusSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

forkEventFixture :: ForkEvent
forkEventFixture = ForkEvent
    { evForkDestination =
        HookRepository
          { whRepoId = 35129393
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterandthehackers/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterandthehackers"
                , whUserId = 7649605
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
                , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = True
          , whRepoUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:30Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterandthehackers/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterandthehackers/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterandthehackers/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = False
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evForkSource =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 1
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evForkSender =
        HookUser
          { whUserLogin = "baxterandthehackers"
          , whUserId = 7649605
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
          , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
          , whUserType = OwnerOrganization
          , whUserIsAdminOfSite = False
          }
    }

gollumEventFixture :: GollumEvent
gollumEventFixture = GollumEvent
    { evGollumPages = V.fromList
        [ HookWikiPage
            { whWikiPageName = "Home"
            , whWikiPageTitle = "Home"
            , whWikiPageSummary = Nothing
            , wkWikiPageAction = "created"
            , whWikiPageSha = "91ea1bd42aa2ba166b86e8aefe049e9837214e67"
            , whWikiPageHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/wiki/Home"
            }
        ]
    , evGollumRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:17Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evGollumSender =
        HookUser
          { whUserLogin = "jasonrudolph"
          , whUserId = 2988
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/2988?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/jasonrudolph"
          , whUserHtmlUrl = URL "https://github.com/jasonrudolph"
          , whUserFollowersUrl = URL "https://api.github.com/users/jasonrudolph/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/jasonrudolph/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/jasonrudolph/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/jasonrudolph/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/jasonrudolph/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/jasonrudolph/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/jasonrudolph/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/jasonrudolph/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/jasonrudolph/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    }

installationEventFixture :: InstallationEvent
installationEventFixture = InstallationEvent
    { evInstallationAction = InstallationDeletedAction
    , evInstallationInfo =
        HookInstallation
          { whInstallationId = 2
          , whInstallationAccount =
              HookUser
                { whUserLogin = "octocat"
                , whUserId = 1
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://github.com/images/error/octocat_happy.gif"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/octocat"
                , whUserHtmlUrl = URL "https://github.com/octocat"
                , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whInstallationRepoSel = "selected"
          , whInstallationTokenUrl = URL "https://api.github.com/installations/2/access_tokens"
          , whInstallationRepoUrl = URL "https://api.github.com/installation/repositories"
          }
    , evInstallationRepos = V.fromList
        [ HookRepositorySimple { whSimplRepoId = 1296269 , whSimplRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg=" , whSimplRepoName = "Hello-World" , whSimplRepoFullName = "octocat/Hello-World", whSimplRepoIsPrivate = True } ]
    , evInstallationSender =
        HookUser
          { whUserLogin = "octocat"
          , whUserId = 1
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://github.com/images/error/octocat_happy.gif"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/octocat"
          , whUserHtmlUrl = URL "https://github.com/octocat"
          , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

installationRepoEventFixture :: InstallationRepositoriesEvent
installationRepoEventFixture = InstallationRepositoriesEvent
    { evInstallationRepoAction = InstallationRepoRemovedAction
    , evInstallationRepoInfo =
        HookInstallation
          { whInstallationId = 2
          , whInstallationAccount =
              HookUser
                { whUserLogin = "octocat"
                , whUserId = 1
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://github.com/images/error/octocat_happy.gif"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/octocat"
                , whUserHtmlUrl = URL "https://github.com/octocat"
                , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whInstallationRepoSel = "selected"
          , whInstallationTokenUrl = URL "https://api.github.com/installations/2/access_tokens"
          , whInstallationRepoUrl = URL "https://api.github.com/installation/repositories"
          }
    , evInstallationRepoSel = "selected"
    , evInstallationReposAdd = V.empty
    , evInstallationReposRemove = V.fromList
        [ HookRepositorySimple { whSimplRepoId = 1296269 , whSimplRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg=" , whSimplRepoName = "Hello-World" , whSimplRepoFullName = "octocat/Hello-World", whSimplRepoIsPrivate = False } ]
    , evInstallationReposSender =
        HookUser
          { whUserLogin = "octocat"
          , whUserId = 1
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://github.com/images/error/octocat_happy.gif"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/octocat"
          , whUserHtmlUrl = URL "https://github.com/octocat"
          , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

issueCommentEventFixture :: IssueCommentEvent
issueCommentEventFixture = IssueCommentEvent
    { evIssueCommentAction = IssueCommentCreatedAction
    , evIssueCommentIssue =
        HookIssue
          { whIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2"
          , whIssueLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/labels{/name}"
          , whIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/comments"
          , whIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/events"
          , whIssueHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/issues/2"
          , whIssueId = 73464126
          , whIssueNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whIssueNumber = 2
          , whIssueTitle = "Spelling error in the README file"
          , whIssueUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whIssueLabels = V.fromList
              [ HookIssueLabels
                  { whIssueLabelId = Nothing
                  , whIssueLabelNodeId = Just "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                  , whIssueLabelUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels/bug"
                  , whIssueLabelName = "bug"
                  , whIssueLabelColor = "fc2929"
                  , whIssueLabelIsDefault = False
                  }
              ]
          , whIssueState = "open"
          , whIssueIsLocked = False
          , whIssueAssignee = Nothing
          , whIssueMilestone = Nothing
          , whIssueCommentCount = 1
          , whIssueCreatedAt = read "2015-05-05 23:40:28Z"
          , whIssueUpdatedAt = read "2015-05-05 23:40:28Z"
          , whIssueClosedAt = Nothing
          , whIssueBody = "It looks like you accidently spelled 'commit' with two 't's."
          }
    , evIssueCommentPayload =
        HookIssueComment
          { whIssueCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments/99262140"
          , whIssueCommentHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/issues/2#issuecomment-99262140"
          , whIssueCommentIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2"
          , whIssueCommentId = 99262140
          , whIssueCommentNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whIssueCommentUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whIssueCommentCreatedAt = read "2015-05-05 23:40:28Z"
          , whIssueCommentUpdatedAt = read "2015-05-05 23:40:28Z"
          , whIssueCommentBody = "You are totally right! I'll get this fixed right away."
          }
    , evIssueCommentRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evIssueCommentSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

issuesEventFixture :: IssuesEvent
issuesEventFixture = IssuesEvent
    { evIssuesEventAction = IssuesOpenedAction
    , evIssuesEventIssue =
        HookIssue
          { whIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2"
          , whIssueLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/labels{/name}"
          , whIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/comments"
          , whIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2/events"
          , whIssueHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/issues/2"
          , whIssueId = 73464126
          , whIssueNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whIssueNumber = 2
          , whIssueTitle = "Spelling error in the README file"
          , whIssueUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whIssueLabels = V.fromList
              [ HookIssueLabels
                  { whIssueLabelId = Just 208045946
                  , whIssueLabelNodeId = Just "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                  , whIssueLabelUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels/bug"
                  , whIssueLabelName = "bug"
                  , whIssueLabelColor = "fc2929"
                  , whIssueLabelIsDefault = True
                  }
              ]
          , whIssueState = "open"
          , whIssueIsLocked = False
          , whIssueAssignee = Nothing
          , whIssueMilestone = Nothing
          , whIssueCommentCount = 0
          , whIssueCreatedAt = read "2015-05-05 23:40:28Z"
          , whIssueUpdatedAt = read "2015-05-05 23:40:28Z"
          , whIssueClosedAt = Nothing
          , whIssueBody = "It looks like you accidently spelled 'commit' with two 't's."
          }
    , evIssuesEventRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evIssuesEventSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

labelEventFixture :: LabelEvent
labelEventFixture = LabelEvent
    { evLabelEventAction = LabelCreatedAction
    , evLabelEventPayload =
        HookRepositoryLabel
          { whRepoLabelNodeId = Just "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoLabelUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/labels/blocked"
          , whRepoLabelName = "blocked"
          , whRepoLabelColor = "ff0000"
          }
    , evLabelEventRepo =
        HookRepository
          { whRepoId = 67075329
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterandthehackers/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterandthehackers"
                , whUserId = 4312013
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
                , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = True
          , whRepoHtmlUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2016-08-31 21:38:51Z"
          , whRepoUpdatedAt = read "2016-08-31 21:38:51Z"
          , whRepoPushedAt = read "2016-08-31 21:38:51Z"
          , whRepoGitUrl = URL "git://github.com/baxterandthehackers/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterandthehackers/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterandthehackers/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = False
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evLabelEventOrganization =
        Just
          HookOrganization
            { whOrgLogin = "baxterandthehackers"
            , whOrgId = 4312013
            , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
            , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
            , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
            , whOrgHooksUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/hooks")
            , whOrgIssuesUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/issues")
            , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
            , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
            , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
            , whOrgDescription = T.empty
            }
    , evLabelEventSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 7649605
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    }

marketplacePurchaseEventFixture :: MarketplacePurchaseEvent
marketplacePurchaseEventFixture = MarketplacePurchaseEvent
    { evMarketplacePurchaseAction = MarketplacePurchaseChangedAction
    , evMarketplacePurchaseEffectiveDate = read "2017-10-25 00:00:00Z"
    , evMarketplacePurchaseSender =
        HookUser
          { whUserLogin = "username"
          , whUserId = 3877742
          , whUserNodeId = "MDEyOk9yZ2FuaXphdGlvbjE="
          , whUserAvatarUrl = URL "https://avatars2.githubusercontent.com/u/3877742?v=4"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/username"
          , whUserHtmlUrl = URL "https://github.com/username"
          , whUserFollowersUrl = URL "https://api.github.com/users/username/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/username/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/username/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/username/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/username/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/username/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/username/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/username/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/username/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    , evMarketplacePurchaseNew =
        HookMarketplacePurchase
          { whMarketplacePurchaseAccount =
              HookMarketplaceAccount
                { whMarketplaceAccountType = OwnerOrganization
                , whMarketplaceAccountId = 18404719
                , whMarketplaceAccountNodeId = "MDEyOk9yZ2FuaXphdGlvbjE="
                , whMarketplaceAccountLogin = "username"
                , whMarketplaceAccountOrganizationBillingEmail = Just "username@email.com"
              }
          , whMarketplacePurchaseBillingCycle = Just HookMarketplaceBillingCycleMonthly
          , whMarketplacePurchaseUnitCount = 10
          , whMarketplacePurchaseOnFreeTrial = False
          , whMarketplacePurchaseFreeTrialEndsOn = Nothing
          , whMarketplacePurchaseNextBillingDate = Just (read "2017-11-05 00:00:00Z")
          , whMarketplacePurchasePlan =
              HookMarketplacePlan
                { whMarketplacePlanId = 435
                , whMarketplacePlanName = "Basic Plan"
                , whMarketplacePlanDescription = "Basic Plan"
                , whMarketplacePlanMonthlyPriceInCents = 1000
                , whMarketplacePlanYearlyPriceInCents = 10000
                , whMarketplacePlanPriceModel = HookMarketplacePlanPriceModelPerUnit
                , whMarketplacePlanHasFreeTrial = True
                , whMarketplacePlanUnitName = Just "seat"
                , whMarketplacePlanBullet = V.fromList ["Is Basic","Because Basic "]
                }
          }
    , evMarketplacePurchasePrevious =
        Just (
          HookMarketplacePurchase
            { whMarketplacePurchaseAccount =
                HookMarketplaceAccount
                  { whMarketplaceAccountType = OwnerOrganization
                  , whMarketplaceAccountId = 18404719
                  , whMarketplaceAccountNodeId = "MDEyOk9yZ2FuaXphdGlvbjE="
                  , whMarketplaceAccountLogin = "username"
                  , whMarketplaceAccountOrganizationBillingEmail = Just "username@email.com"
                  }
              , whMarketplacePurchaseBillingCycle = Just HookMarketplaceBillingCycleMonthly
              , whMarketplacePurchaseUnitCount = 1
              , whMarketplacePurchaseOnFreeTrial = False
              , whMarketplacePurchaseFreeTrialEndsOn = Nothing
              , whMarketplacePurchaseNextBillingDate = Nothing
              , whMarketplacePurchasePlan =
                  HookMarketplacePlan
                    { whMarketplacePlanId = 435
                    , whMarketplacePlanName = "Basic Plan"
                    , whMarketplacePlanDescription = "Basic Plan"
                    , whMarketplacePlanMonthlyPriceInCents = 1000
                    , whMarketplacePlanYearlyPriceInCents = 10000
                    , whMarketplacePlanPriceModel = HookMarketplacePlanPriceModelPerUnit
                    , whMarketplacePlanHasFreeTrial = True
                    , whMarketplacePlanUnitName = Just "seat"
                    , whMarketplacePlanBullet = V.fromList ["Is Basic","Because Basic "]
                    }
            })
    }

memberEventFixture :: MemberEvent
memberEventFixture = MemberEvent
    { evMemberAction = MemberAddedAction
    , evMemberUser =
        HookUser
          { whUserLogin = "octocat"
          , whUserId = 583231
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/583231?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/octocat"
          , whUserHtmlUrl = URL "https://github.com/octocat"
          , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evMemberRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:40Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evMemberSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

membershipEventFixture :: MembershipEvent
membershipEventFixture = MembershipEvent
    { evMembershipAction = MembershipAddedAction
    , evMembershipScope = "team"
    , evMembershipUser =
        HookUser
          { whUserLogin = "kdaigle"
          , whUserId = 2501
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/2501?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/kdaigle"
          , whUserHtmlUrl = URL "https://github.com/kdaigle"
          , whUserFollowersUrl = URL "https://api.github.com/users/kdaigle/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/kdaigle/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/kdaigle/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/kdaigle/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/kdaigle/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/kdaigle/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/kdaigle/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/kdaigle/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/kdaigle/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    , evMembershipTeam =
        HookTeam
          { whTeamName = "Contractors"
          , whTeamId = 123456
          , whTeamNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whTeamSlug = "contractors"
          , whTeamPermission = "admin"
          , whTeamUrl = URL "https://api.github.com/teams/123456"
          , whTeamMembersUrl = URL "https://api.github.com/teams/123456/members{/member}"
          , whTeamRepositoriesUrl = URL "https://api.github.com/teams/123456/repos"
          }
    , evMembershipOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 7649605
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Nothing
          , whOrgIssuesUrl = Nothing
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=2"
          , whOrgDescription = T.empty
          }
    , evMembershipSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

milestoneEventFixture :: MilestoneEvent
milestoneEventFixture = MilestoneEvent
    { evMilestoneAction = MilestoneCreatedAction
    , evMilestoenPayload =
        HookMilestone
          { whMilestoneUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones/3"
          , whMilestoneHtmlUrl =
              URL "https://github.com/baxterandthehackers/public-repo/milestones/Test%20milestone%20creation%20webhook%20from%20command%20line2"
          , whMilestoneLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones/3/labels"
          , whMilestoneId = 2055681
          , whMilestoneNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whMilestoneNumber = 3
          , whMilestoneTitle = "I am a milestone"
          , whMilestoneDescription = Nothing
          , whMilestoneCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 7649605
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = True
                }
          , whMilestoneOpenIssues = 0
          , whMilestoneClosedIssues = 0
          , whMilestoneState = "open"
          , whMilestoneCreatedAt = read "2016-10-07 19:26:08Z"
          , whMilestoneUpdatedAt = read "2016-10-07 19:26:08Z"
          , whMilestoneDueOn = Nothing
          , whMilestoneClosedAt = Nothing
          }
    , evMilestoneRepo =
        HookRepository
          { whRepoId = 70275481
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterandthehackers/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterandthehackers"
                , whUserId = 4312013
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
                , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = True
          , whRepoHtmlUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2016-10-07 19:10:12Z"
          , whRepoUpdatedAt = read "2016-10-07 19:10:12Z"
          , whRepoPushedAt = read "2016-10-07 19:10:13Z"
          , whRepoGitUrl = URL "git://github.com/baxterandthehackers/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterandthehackers/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterandthehackers/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = False
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evMilestoneOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 4312013
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/hooks")
          , whOrgIssuesUrl = Just (URL "https://api.github.com/orgs/baxterandthehackers/issues")
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
          , whOrgDescription = T.empty
          }
    , evMilestoneSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 7649605
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    }

orgBlockEventFixture :: OrgBlockEvent
orgBlockEventFixture = OrgBlockEvent
    { evOrgBlockAction = OrgBlockBlockedAction
    , evOrgBlockUser =
        HookUser
          { whUserLogin = "octocat"
          , whUserId = 583231
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/583231?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/octocat"
          , whUserHtmlUrl = URL "https://github.com/octocat"
          , whUserFollowersUrl = URL "https://api.github.com/users/octocat/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/octocat/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/octocat/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/octocat/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/octocat/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/octocat/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/octocat/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/octocat/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/octocat/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evOrgBlockOrg =
        HookOrganization
          { whOrgLogin = "github"
          , whOrgId = 4366038
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/github"
          , whOrgReposUrl = URL "https://api.github.com/orgs/github/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/github/events"
          , whOrgHooksUrl = Just $ URL "https://api.github.com/orgs/github/hooks"
          , whOrgIssuesUrl = Just $ URL "https://api.github.com/orgs/github/issues"
          , whOrgMembersUrl = URL "https://api.github.com/orgs/github/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/github/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4366038?v=3"
          , whOrgDescription = T.empty
          }
    , evOrgBlockSender =
        HookUser
          { whUserLogin = "octodocs"
          , whUserId = 25781999
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/25781999?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/octodocs"
          , whUserHtmlUrl = URL "https://github.com/octodocs"
          , whUserFollowersUrl = URL "https://api.github.com/users/octodocs/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/octodocs/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/octodocs/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/octodocs/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/octodocs/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/octodocs/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/octodocs/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/octodocs/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/octodocs/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

organizationEventFixture :: OrganizationEvent
organizationEventFixture = OrganizationEvent
    { evOrganizationAction = OrgMemberInvitedAction
    , evOrganizationInvitation =
        Just $ HookOrganizationInvitation
          { whOrgInvitationId = 3294302
          , whOrgInvitationNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgInvitationLogin = "baxterthehacker"
          , whOrgInvitationEmail = Nothing
          , whOrgInvitationRole = "direct_member"
          }
    , evOrganizationMembership =
        Just $ HookOrganizationMembership
          { whOrgMembershipUrl = URL "https://api.github.com/orgs/baxterandthehackers/memberships/baxterthehacker"
          , whOrgMembershipState = "active"
          , whOrgMembershipRole = "member"
          , whOrgMembershipOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgMembershipUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 7649605
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/17085448?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          }
    , evOrganizationOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 4312013
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Just $ URL "https://api.github.com/orgs/baxterandthehackers/hooks"
          , whOrgIssuesUrl = Just $ URL "https://api.github.com/orgs/baxterandthehackers/issues"
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
          , whOrgDescription = T.empty
          }
    , evOrganizationSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 7649605
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = True
          }
    }

pageBuildEventFixture :: PageBuildEvent
pageBuildEventFixture = PageBuildEvent
    { evPageBuildId = 15995382
    , evPageBuildResult =
        HookPageBuildResult
          { whPageBuildUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pages/builds/15995382"
          , whPageBuildStatus = "built"
          , whPageBuildError = Nothing
          , whPageBuildPusher =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whPageBuildCommitSha = "053b99542c83021d6b202d1a1f5ecd5ef7084e55"
          , whPageBuildDuration = 3790
          , whPageBuildCreatedAt = read "2015-05-05 23:40:13Z"
          , whPageBuildUpdatedAt = read "2015-05-05 23:40:17Z"
          }
    , evPageBuildRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:17Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evPageBuildSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

projectCardEventFixture :: ProjectCardEvent
projectCardEventFixture = ProjectCardEvent
    { evProjectCardAction = ProjectCardCreatedAction
    , evProjectCardPayload =
        HookProjectCard
          { whProjectCardUrl = URL "https://api.github.com/projects/columns/cards/1266091"
          , whProjectCardColumnUrl = URL "https://api.github.com/projects/columns/515520"
          , whProjectCardColumnId = 515520
          , whProjectCardId = 1266091
          , whProjectCardNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whProjectCardNote = Nothing
          , whProjectCardCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whProjectCardCreatedAt = read "2017-09-27 23:37:43Z"
          , whProjectCardUpdatedAt = read "2017-09-27 23:39:09Z"
          , whProjectCardContentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/2"
          }
    , evProjectCardRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evProjectCardOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 7649605
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Nothing
          , whOrgIssuesUrl = Nothing
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=2"
          , whOrgDescription = T.empty
          }
    , evProjectCardSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

projectColumnEventFixture :: ProjectColumnEvent
projectColumnEventFixture = ProjectColumnEvent
    { evProjectColumnAction = ProjectColumnCreatedAction
    , evProjectColumnPayload =
        HookProjectColumn
          { whProjectColumnUrl = URL "https://api.github.com/projects/columns/515520"
          , whProjectColumnProjUrl = URL "https://api.github.com/projects/288065"
          , whProjectColumnCardsUrl = URL "https://api.github.com/projects/columns/515520/cards"
          , whProjectColumnId = 515520
          , whProjectColumnNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whProjectColumnName = "High Priority"
          , whProjectColumnCreatedAt = read "2017-09-27 23:37:43Z"
          , whProjectColumnUpdatedAt = read "2017-09-27 23:39:09Z"
          }
    , evProjectColumnRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evProjectColumnOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 7649605
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Nothing
          , whOrgIssuesUrl = Nothing
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=2"
          , whOrgDescription = T.empty
          }
    , evProjectColumnSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

projectEventFixture :: ProjectEvent
projectEventFixture = ProjectEvent
    { evProjectEventAction = ProjectCreatedAction
    , evProjectPayload =
        HookProject
          { whProjectOwnerUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whProjectUrl = URL "https://api.github.com/projects/288065"
          , whProjectColumnsUrl = URL "https://api.github.com/projects/288065/columns"
          , whProjectId = 288065
          , whProjectNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whProjectName = "2017"
          , whProjectBody = "Roadmap for work to be done in 2017"
          , whProjectNumber = 10
          , whProjectState = "open"
          , whProjectCreator =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whProjectCreatedAt = read "2017-09-27 23:37:43Z"
          , whProjectUpdatedAt = read "2017-09-27 23:39:09Z"
          }
    , evProjectRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evProjectOrganization =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 7649605
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Nothing
          , whOrgIssuesUrl = Nothing
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=2"
          , whOrgDescription = T.empty
          }
    , evProjectSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

publicEventFixture :: PublicEvent
publicEventFixture = PublicEvent
    { evPublicEventRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:41Z"
          , whRepoPushedAt = read "2015-05-05 23:40:40Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evPublicEventSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

pullRequestEventFixture :: PullRequestEvent
pullRequestEventFixture = PullRequestEvent
    { evPullReqAction = PullRequestOpenedAction
    , evPullReqNumber = 1
    , evPullReqPayload =
        HookPullRequest
          { whPullReqUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
          , whPullReqId = 34778301
          , whPullReqNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whPullReqHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1"
          , whPullReqDiffUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.diff"
          , whPullReqPatchUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.patch"
          , whPullReqIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1"
          , whPullReqNumber = 1
          , whPullReqState = "open"
          , whPullReqIsLocked = False
          , whPullReqTitle = "Update the README with new information"
          , whPullReqUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whPullReqBody = "This is a pretty simple change that we need to pull into master."
          , whPullReqCreatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqUpdatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqClosedAt = Nothing
          , whPullReqMergedAt = Nothing
          , whPullReqMergeCommitSha = Nothing
          , whPullReqAssignee = Nothing
          , whPullReqMilestone = Nothing
          , whPullReqCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/commits"
          , whPullReqRevCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/comments"
          , whPullReqRevCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/comments{/number}"
          , whPullReqCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1/comments"
          , whPullReqStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
          , whPullReqBase =
              PullRequestTarget
                { whPullReqTargetSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:26Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:master"
                , whPullReqTargetRef = "master"
                }
          , whPullReqHead =
              PullRequestTarget
                { whPullReqTargetSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:26Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:changes"
                , whPullReqTargetRef = "changes"
                }
          , whPullReqMergeableState = Just "unknown"
          , whPullReqCommentCount = Just 0
          , whPullReqRevCommentCount = Just 0
          , whPullReqCommitCount = Just 1
          , whPullReqAdditionsCount = Just 1
          , whPullReqDeletionsCount = Just 1
          , whPullReqFileChangeCount = Just 1
          }
    , evPullReqRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:26Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 1
          , whRepoDefaultBranchName = "master"
          }
    , evPullReqSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evPullReqInstallationId = Just 234
    }

pullRequestEventNullBodyFixture :: PullRequestEvent
pullRequestEventNullBodyFixture = PullRequestEvent
    { evPullReqAction = PullRequestOpenedAction
    , evPullReqNumber = 1
    , evPullReqPayload =
        HookPullRequest
          { whPullReqUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
          , whPullReqId = 34778301
          , whPullReqNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whPullReqHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1"
          , whPullReqDiffUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.diff"
          , whPullReqPatchUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.patch"
          , whPullReqIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1"
          , whPullReqNumber = 1
          , whPullReqState = "open"
          , whPullReqIsLocked = False
          , whPullReqTitle = "Update the README with new information"
          , whPullReqUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whPullReqBody = ""
          , whPullReqCreatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqUpdatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqClosedAt = Nothing
          , whPullReqMergedAt = Nothing
          , whPullReqMergeCommitSha = Nothing
          , whPullReqAssignee = Nothing
          , whPullReqMilestone = Nothing
          , whPullReqCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/commits"
          , whPullReqRevCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/comments"
          , whPullReqRevCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/comments{/number}"
          , whPullReqCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1/comments"
          , whPullReqStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
          , whPullReqBase =
              PullRequestTarget
                { whPullReqTargetSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:26Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:master"
                , whPullReqTargetRef = "master"
                }
          , whPullReqHead =
              PullRequestTarget
                { whPullReqTargetSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:26Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:changes"
                , whPullReqTargetRef = "changes"
                }
          , whPullReqMergeableState = Just "unknown"
          , whPullReqCommentCount = Just 0
          , whPullReqRevCommentCount = Just 0
          , whPullReqCommitCount = Just 1
          , whPullReqAdditionsCount = Just 1
          , whPullReqDeletionsCount = Just 1
          , whPullReqFileChangeCount = Just 1
          }
    , evPullReqRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:26Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 1
          , whRepoDefaultBranchName = "master"
          }
    , evPullReqSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    , evPullReqInstallationId = Just 234
    }

pullRequestEventDeleteNullHeadRepoAnomalyFixture :: PullRequestEvent
pullRequestEventDeleteNullHeadRepoAnomalyFixture =
  PullRequestEvent
    { evPullReqAction = PullRequestClosedAction,
      evPullReqNumber = 254,
      evPullReqPayload =
        HookPullRequest
          { whPullReqUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls/254",
            whPullReqId = 57285673867,
            whPullReqNodeId = "PR_kwODAywNcw4ukzp6",
            whPullReqHtmlUrl = URL "https://github.com/upstreamowner/therepo/pull/254",
            whPullReqDiffUrl = URL "https://github.com/upstreamowner/therepo/pull/254.diff",
            whPullReqPatchUrl = URL "https://github.com/upstreamowner/therepo/pull/254.patch",
            whPullReqIssueUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/254",
            whPullReqNumber = 254,
            whPullReqState = "closed",
            whPullReqIsLocked = False,
            whPullReqTitle = "This was a PR title",
            whPullReqUser =
              HookUser
                { whUserLogin = "thecontributor",
                  whUserId = 8425382758,
                  whUserNodeId = "MDV6XQlcQjN1EjNNQ4T3",
                  whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/8425382758?v=4",
                  whUserGravatarId = URL "",
                  whUserUrl = URL "https://api.github.com/users/thecontributor",
                  whUserHtmlUrl = URL "https://github.com/thecontributor",
                  whUserFollowersUrl = URL "https://api.github.com/users/thecontributor/followers",
                  whUserFollowingUrl = URL "https://api.github.com/users/thecontributor/following{/other_user}",
                  whUserGistsUrl = URL "https://api.github.com/users/thecontributor/gists{/gist_id}",
                  whUserStarredUrl = URL "https://api.github.com/users/thecontributor/starred{/owner}{/repo}",
                  whUserSubscriptionsUrl = URL "https://api.github.com/users/thecontributor/subscriptions",
                  whUserOrganizationsUrl = URL "https://api.github.com/users/thecontributor/orgs",
                  whUserReposUrl = URL "https://api.github.com/users/thecontributor/repos",
                  whUserEventsUrl = URL "https://api.github.com/users/thecontributor/events{/privacy}",
                  whUserReceivedEventsUrl = URL "https://api.github.com/users/thecontributor/received_events",
                  whUserType = OwnerUser,
                  whUserIsAdminOfSite = False
                },
            whPullReqBody = "",
            whPullReqCreatedAt = read "2021-11-20 20:00:00Z",
            whPullReqUpdatedAt = read "2021-11-20 20:00:00Z",
            whPullReqClosedAt = Just $ read "2021-11-20 20:00:00Z",
            whPullReqMergedAt = Nothing,
            whPullReqMergeCommitSha = Nothing,
            whPullReqAssignee = Nothing,
            whPullReqMilestone = Nothing,
            whPullReqCommitsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls/254/commits",
            whPullReqRevCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls/254/comments",
            whPullReqRevCommentUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls/comments{/number}",
            whPullReqCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/254/comments",
            whPullReqStatusesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/statuses/b31c9f3501a256e212364c839050736f49e5fa6e",
            whPullReqBase =
              PullRequestTarget
                { whPullReqTargetSha = "a42211bde42a98b41bc673f4e90f7b01b41fde5a",
                  whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "upstreamowner",
                        whUserId = 23804562983764,
                        whUserNodeId = "MDEykO9yZF2auXpdhGvlbjEM2kT3TOQ3",
                        whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/23804562983764?v=4",
                        whUserGravatarId = URL "",
                        whUserUrl = URL "https://api.github.com/users/upstreamowner",
                        whUserHtmlUrl = URL "https://github.com/upstreamowner",
                        whUserFollowersUrl = URL "https://api.github.com/users/upstreamowner/followers",
                        whUserFollowingUrl = URL "https://api.github.com/users/upstreamowner/following{/other_user}",
                        whUserGistsUrl = URL "https://api.github.com/users/upstreamowner/gists{/gist_id}",
                        whUserStarredUrl = URL "https://api.github.com/users/upstreamowner/starred{/owner}{/repo}",
                        whUserSubscriptionsUrl = URL "https://api.github.com/users/upstreamowner/subscriptions",
                        whUserOrganizationsUrl = URL "https://api.github.com/users/upstreamowner/orgs",
                        whUserReposUrl = URL "https://api.github.com/users/upstreamowner/repos",
                        whUserEventsUrl = URL "https://api.github.com/users/upstreamowner/events{/privacy}",
                        whUserReceivedEventsUrl = URL "https://api.github.com/users/upstreamowner/received_events",
                        whUserType = OwnerOrganization,
                        whUserIsAdminOfSite = False
                      },
                  whPullReqTargetRepo =
                    Just
                      ( HookRepository
                          { whRepoId = 53218753,
                            whRepoNodeId = "MDEwOlJlcG9zaXRvcnk1MzIxODc1Mw==",
                            whRepoName = "therepo",
                            whRepoFullName = "upstreamowner/therepo",
                            whRepoOwner =
                              Right
                                ( HookUser
                                    { whUserLogin = "upstreamowner",
                                      whUserId = 23804562983764,
                                      whUserNodeId = "MDEyOk9yZ2FuaXphdGlvbjE2MTk3OTQ3",
                                      whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/23804562983764?v=4",
                                      whUserGravatarId = URL "",
                                      whUserUrl = URL "https://api.github.com/users/upstreamowner",
                                      whUserHtmlUrl = URL "https://github.com/upstreamowner",
                                      whUserFollowersUrl = URL "https://api.github.com/users/upstreamowner/followers",
                                      whUserFollowingUrl = URL "https://api.github.com/users/upstreamowner/following{/other_user}",
                                      whUserGistsUrl = URL "https://api.github.com/users/upstreamowner/gists{/gist_id}",
                                      whUserStarredUrl = URL "https://api.github.com/users/upstreamowner/starred{/owner}{/repo}",
                                      whUserSubscriptionsUrl = URL "https://api.github.com/users/upstreamowner/subscriptions",
                                      whUserOrganizationsUrl = URL "https://api.github.com/users/upstreamowner/orgs",
                                      whUserReposUrl = URL "https://api.github.com/users/upstreamowner/repos",
                                      whUserEventsUrl = URL "https://api.github.com/users/upstreamowner/events{/privacy}",
                                      whUserReceivedEventsUrl = URL "https://api.github.com/users/upstreamowner/received_events",
                                      whUserType = OwnerOrganization,
                                      whUserIsAdminOfSite = False
                                    }
                                ),
                            whRepoIsPrivate = False,
                            whRepoHtmlUrl = URL "https://github.com/upstreamowner/therepo",
                            whRepoDescription = "Build scripts for the server",
                            whRepoIsAFork = False,
                            whRepoUrl = URL "https://api.github.com/repos/upstreamowner/therepo",
                            whRepoForksUrl = URL "https://api.github.com/repos/upstreamowner/therepo/forks",
                            whRepoKeysUrl = URL "https://api.github.com/repos/upstreamowner/therepo/keys{/key_id}",
                            whRepoCollaboratorsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/collaborators{/collaborator}",
                            whRepoTeamsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/teams",
                            whRepoHooksUrl = URL "https://api.github.com/repos/upstreamowner/therepo/hooks",
                            whRepoIssueEventsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/events{/number}",
                            whRepoEventsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/events",
                            whRepoAssigneesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/assignees{/user}",
                            whRepoBranchesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/branches{/branch}",
                            whRepoTagsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/tags",
                            whRepoBlobsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/blobs{/sha}",
                            whRepoGitTagsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/tags{/sha}",
                            whRepoGitRefsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/refs{/sha}",
                            whRepoTreesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/trees{/sha}",
                            whRepoStatusesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/statuses/{sha}",
                            whRepoLanguagesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/languages",
                            whRepoStargazersUrl = URL "https://api.github.com/repos/upstreamowner/therepo/stargazers",
                            whRepoContributorsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/contributors",
                            whRepoSubscribersUrl = URL "https://api.github.com/repos/upstreamowner/therepo/subscribers",
                            whRepoSubscriptionUrl = URL "https://api.github.com/repos/upstreamowner/therepo/subscription",
                            whRepoCommitsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/commits{/sha}",
                            whRepoGitCommitsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/commits{/sha}",
                            whRepoCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/comments{/number}",
                            whRepoIssueCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/comments{/number}",
                            whRepoContentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/contents/{+path}",
                            whRepoCompareUrl = URL "https://api.github.com/repos/upstreamowner/therepo/compare/{base}...{head}",
                            whRepoMergesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/merges",
                            whRepoArchiveUrl = URL "https://api.github.com/repos/upstreamowner/therepo/{archive_format}{/ref}",
                            whRepoDownloadsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/downloads",
                            whRepoIssuesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues{/number}",
                            whRepoPullsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls{/number}",
                            whRepoMilestonesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/milestones{/number}",
                            whRepoNotificationsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/notifications{?since,all,participating}",
                            whRepoLabelsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/labels{/name}",
                            whRepoReleasesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/releases{/id}",
                            whRepoCreatedAt = read "2016-01-01 00:00:00Z",
                            whRepoUpdatedAt = read "2021-11-13 22:00:00Z",
                            whRepoPushedAt = read "2021-11-20 20:00:00Z",
                            whRepoGitUrl = URL "git://github.com/upstreamowner/therepo.git",
                            whRepoSshUrl = URL "git@github.com:upstreamowner/therepo.git",
                            whRepoCloneUrl = URL "https://github.com/upstreamowner/therepo.git",
                            whRepoSvnUrl = URL "https://github.com/upstreamowner/therepo",
                            whRepoHomepage = Just (URL ""),
                            whRepoSize = 429518,
                            whRepoStargazersCount = 8,
                            whRepoWatchersCount = 8,
                            whRepoLanguage = Just "SomeLanguage",
                            whRepoHasIssues = True,
                            whRepoHasDownloads = True,
                            whRepoHasWiki = True,
                            whRepoHasPages = False,
                            whRepoForkCount = 17,
                            whRepoMirrorUrl = Nothing,
                            whRepoOpenIssuesCount = 3,
                            whRepoDefaultBranchName = "master"
                          }
                      ),
                  whPullReqTargetLabel = "upstreamowner:master",
                  whPullReqTargetRef = "master"
                },
            whPullReqHead =
              PullRequestTarget
                { whPullReqTargetSha = "b319cf3510a25e621236c483950073f6495ef6ae",
                  whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "thecontributor",
                        whUserId = 8425382758,
                        whUserNodeId = "MDQ6VXNlcjQ1NjE4NTQ3",
                        whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/8425382758?v=4",
                        whUserGravatarId = URL "",
                        whUserUrl = URL "https://api.github.com/users/thecontributor",
                        whUserHtmlUrl = URL "https://github.com/thecontributor",
                        whUserFollowersUrl = URL "https://api.github.com/users/thecontributor/followers",
                        whUserFollowingUrl = URL "https://api.github.com/users/thecontributor/following{/other_user}",
                        whUserGistsUrl = URL "https://api.github.com/users/thecontributor/gists{/gist_id}",
                        whUserStarredUrl = URL "https://api.github.com/users/thecontributor/starred{/owner}{/repo}",
                        whUserSubscriptionsUrl = URL "https://api.github.com/users/thecontributor/subscriptions",
                        whUserOrganizationsUrl = URL "https://api.github.com/users/thecontributor/orgs",
                        whUserReposUrl = URL "https://api.github.com/users/thecontributor/repos",
                        whUserEventsUrl = URL "https://api.github.com/users/thecontributor/events{/privacy}",
                        whUserReceivedEventsUrl = URL "https://api.github.com/users/thecontributor/received_events",
                        whUserType = OwnerUser,
                        whUserIsAdminOfSite = False
                      },
                  whPullReqTargetRepo = Nothing,
                  whPullReqTargetLabel = "thecontributor:master",
                  whPullReqTargetRef = "master"
                },
            whPullReqMergeableState = Just "dirty",
            whPullReqCommentCount = Just 2,
            whPullReqRevCommentCount = Just 0,
            whPullReqCommitCount = Just 4,
            whPullReqAdditionsCount = Just 21424,
            whPullReqDeletionsCount = Just 0,
            whPullReqFileChangeCount = Just 1605
          },
      evPullReqRepo =
        HookRepository
          { whRepoId = 53218753,
            whRepoNodeId = "MDEwOlJlcG9zaXRvcnk1MzIxODc1Mw==",
            whRepoName = "therepo",
            whRepoFullName = "upstreamowner/therepo",
            whRepoOwner =
              Right
                ( HookUser
                    { whUserLogin = "upstreamowner",
                      whUserId = 23804562983764,
                      whUserNodeId = "MDEyOk9yZ2FuaXphdGlvbjE2MTk3OTQ3",
                      whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/23804562983764?v=4",
                      whUserGravatarId = URL "",
                      whUserUrl = URL "https://api.github.com/users/upstreamowner",
                      whUserHtmlUrl = URL "https://github.com/upstreamowner",
                      whUserFollowersUrl = URL "https://api.github.com/users/upstreamowner/followers",
                      whUserFollowingUrl = URL "https://api.github.com/users/upstreamowner/following{/other_user}",
                      whUserGistsUrl = URL "https://api.github.com/users/upstreamowner/gists{/gist_id}",
                      whUserStarredUrl = URL "https://api.github.com/users/upstreamowner/starred{/owner}{/repo}",
                      whUserSubscriptionsUrl = URL "https://api.github.com/users/upstreamowner/subscriptions",
                      whUserOrganizationsUrl = URL "https://api.github.com/users/upstreamowner/orgs",
                      whUserReposUrl = URL "https://api.github.com/users/upstreamowner/repos",
                      whUserEventsUrl = URL "https://api.github.com/users/upstreamowner/events{/privacy}",
                      whUserReceivedEventsUrl = URL "https://api.github.com/users/upstreamowner/received_events",
                      whUserType = OwnerOrganization,
                      whUserIsAdminOfSite = False
                    }
                ),
            whRepoIsPrivate = False,
            whRepoHtmlUrl = URL "https://github.com/upstreamowner/therepo",
            whRepoDescription = "Build scripts for the server",
            whRepoIsAFork = False,
            whRepoUrl = URL "https://api.github.com/repos/upstreamowner/therepo",
            whRepoForksUrl = URL "https://api.github.com/repos/upstreamowner/therepo/forks",
            whRepoKeysUrl = URL "https://api.github.com/repos/upstreamowner/therepo/keys{/key_id}",
            whRepoCollaboratorsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/collaborators{/collaborator}",
            whRepoTeamsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/teams",
            whRepoHooksUrl = URL "https://api.github.com/repos/upstreamowner/therepo/hooks",
            whRepoIssueEventsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/events{/number}",
            whRepoEventsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/events",
            whRepoAssigneesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/assignees{/user}",
            whRepoBranchesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/branches{/branch}",
            whRepoTagsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/tags",
            whRepoBlobsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/blobs{/sha}",
            whRepoGitTagsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/tags{/sha}",
            whRepoGitRefsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/refs{/sha}",
            whRepoTreesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/trees{/sha}",
            whRepoStatusesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/statuses/{sha}",
            whRepoLanguagesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/languages",
            whRepoStargazersUrl = URL "https://api.github.com/repos/upstreamowner/therepo/stargazers",
            whRepoContributorsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/contributors",
            whRepoSubscribersUrl = URL "https://api.github.com/repos/upstreamowner/therepo/subscribers",
            whRepoSubscriptionUrl = URL "https://api.github.com/repos/upstreamowner/therepo/subscription",
            whRepoCommitsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/commits{/sha}",
            whRepoGitCommitsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/git/commits{/sha}",
            whRepoCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/comments{/number}",
            whRepoIssueCommentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues/comments{/number}",
            whRepoContentsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/contents/{+path}",
            whRepoCompareUrl = URL "https://api.github.com/repos/upstreamowner/therepo/compare/{base}...{head}",
            whRepoMergesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/merges",
            whRepoArchiveUrl = URL "https://api.github.com/repos/upstreamowner/therepo/{archive_format}{/ref}",
            whRepoDownloadsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/downloads",
            whRepoIssuesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/issues{/number}",
            whRepoPullsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/pulls{/number}",
            whRepoMilestonesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/milestones{/number}",
            whRepoNotificationsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/notifications{?since,all,participating}",
            whRepoLabelsUrl = URL "https://api.github.com/repos/upstreamowner/therepo/labels{/name}",
            whRepoReleasesUrl = URL "https://api.github.com/repos/upstreamowner/therepo/releases{/id}",
            whRepoCreatedAt = read "2016-01-01 00:00:00Z",
            whRepoUpdatedAt = read "2021-11-13 22:00:00Z",
            whRepoPushedAt = read "2021-11-20 20:00:00Z",
            whRepoGitUrl = URL "git://github.com/upstreamowner/therepo.git",
            whRepoSshUrl = URL "git@github.com:upstreamowner/therepo.git",
            whRepoCloneUrl = URL "https://github.com/upstreamowner/therepo.git",
            whRepoSvnUrl = URL "https://github.com/upstreamowner/therepo",
            whRepoHomepage = Just (URL ""),
            whRepoSize = 429518,
            whRepoStargazersCount = 8,
            whRepoWatchersCount = 8,
            whRepoLanguage = Just "SomeLanguage",
            whRepoHasIssues = True,
            whRepoHasDownloads = True,
            whRepoHasWiki = True,
            whRepoHasPages = False,
            whRepoForkCount = 17,
            whRepoMirrorUrl = Nothing,
            whRepoOpenIssuesCount = 3,
            whRepoDefaultBranchName = "master"
          },
      evPullReqSender =
        HookUser
          { whUserLogin = "thesender",
            whUserId = 3290456203984,
            whUserNodeId = "MDQV6XlNcjwIzNkMzjQ=",
            whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/3290456203984?v=4",
            whUserGravatarId = URL "",
            whUserUrl = URL "https://api.github.com/users/thesender",
            whUserHtmlUrl = URL "https://github.com/thesender",
            whUserFollowersUrl = URL "https://api.github.com/users/thesender/followers",
            whUserFollowingUrl = URL "https://api.github.com/users/thesender/following{/other_user}",
            whUserGistsUrl = URL "https://api.github.com/users/thesender/gists{/gist_id}",
            whUserStarredUrl = URL "https://api.github.com/users/thesender/starred{/owner}{/repo}",
            whUserSubscriptionsUrl = URL "https://api.github.com/users/thesender/subscriptions",
            whUserOrganizationsUrl = URL "https://api.github.com/users/thesender/orgs",
            whUserReposUrl = URL "https://api.github.com/users/thesender/repos",
            whUserEventsUrl = URL "https://api.github.com/users/thesender/events{/privacy}",
            whUserReceivedEventsUrl = URL "https://api.github.com/users/thesender/received_events",
            whUserType = OwnerUser,
            whUserIsAdminOfSite = False
          },
      evPullReqInstallationId = Just 6678514558
    }


pullRequestReviewCommentEventFixture :: PullRequestReviewCommentEvent
pullRequestReviewCommentEventFixture = PullRequestReviewCommentEvent
    { evPullReqRevComAction = PullRequestReviewCommentCreatedAction
    , evPullReqRevComment =
        HookPullRequestReviewComment
          { whPullReqRevComUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/comments/29724692"
          , whPullReqRevComId = 29724692
          , whPullReqRevComNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whPullReqRevComDiffHunk = "@@ -1 +1 @@\n-# public-repo"
          , whPullReqRevComPath = "README.md"
          , whPullReqRevComPos = 1
          , whPullReqRevComOrigPos = 1
          , whPullReqRevComCommitSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
          , whPullReqRevComOrigSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
          , whPullReqRevComUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whPullReqRevComBody = "Maybe you should use more emojji on this line."
          , whPullReqRevComCreatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqRevComUpdatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqRevComHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1#discussion_r29724692"
          , whPullReqRevComPullReqUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
          }
    , evPullReqRevTarget =
        HookPullRequest
          { whPullReqUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1"
          , whPullReqId = 34778301
          , whPullReqNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whPullReqHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1"
          , whPullReqDiffUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.diff"
          , whPullReqPatchUrl = URL "https://github.com/baxterthehacker/public-repo/pull/1.patch"
          , whPullReqIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1"
          , whPullReqNumber = 1
          , whPullReqState = "open"
          , whPullReqIsLocked = False
          , whPullReqTitle = "Update the README with new information"
          , whPullReqUser =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whPullReqBody = "This is a pretty simple change that we need to pull into master."
          , whPullReqCreatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqUpdatedAt = read "2015-05-05 23:40:27Z"
          , whPullReqClosedAt = Nothing
          , whPullReqMergedAt = Nothing
          , whPullReqMergeCommitSha = Just "18721552ba489fb84e12958c1b5694b5475f7991"
          , whPullReqAssignee = Nothing
          , whPullReqMilestone = Nothing
          , whPullReqCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/commits"
          , whPullReqRevCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/1/comments"
          , whPullReqRevCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/comments{/number}"
          , whPullReqCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/1/comments"
          , whPullReqStatusesUrl =
              URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
          , whPullReqCommentCount = Nothing
          , whPullReqRevCommentCount = Nothing
          , whPullReqCommitCount = Nothing
          , whPullReqAdditionsCount = Nothing
          , whPullReqDeletionsCount = Nothing
          , whPullReqFileChangeCount = Nothing
          , whPullReqBase =
              PullRequestTarget
                { whPullReqTargetSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:27Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:master"
                , whPullReqTargetRef = "master"
                }
          , whPullReqHead =
              PullRequestTarget
                { whPullReqTargetSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
                , whPullReqTargetUser =
                    HookUser
                      { whUserLogin = "baxterthehacker"
                      , whUserId = 6752317
                      , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                      , whUserGravatarId = URL ""
                      , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                      , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                      , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                      , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                      , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                      , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                      , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                      , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                      , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                      , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                      , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                      , whUserType = OwnerUser
                      , whUserIsAdminOfSite = False
                      }
                , whPullReqTargetRepo =
                    Just HookRepository
                      { whRepoId = 35129377
                      , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                      , whRepoName = "public-repo"
                      , whRepoFullName = "baxterthehacker/public-repo"
                      , whRepoOwner =
                          Right
                            HookUser
                              { whUserLogin = "baxterthehacker"
                              , whUserId = 6752317
                              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                              , whUserGravatarId = URL ""
                              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                              , whUserType = OwnerUser
                              , whUserIsAdminOfSite = False
                              }
                      , whRepoIsPrivate = False
                      , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoDescription = ""
                      , whRepoIsAFork = False
                      , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                      , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                      , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                      , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                      , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                      , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                      , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                      , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                      , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                      , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                      , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                      , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                      , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                      , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                      , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                      , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                      , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                      , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                      , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                      , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                      , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                      , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                      , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                      , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                      , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                      , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                      , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                      , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                      , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                      , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                      , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                      , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                      , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                      , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                      , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                      , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                      , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
                      , whRepoPushedAt = read "2015-05-05 23:40:27Z"
                      , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                      , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                      , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                      , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                      , whRepoHomepage = Nothing
                      , whRepoSize = 0
                      , whRepoStargazersCount = 0
                      , whRepoWatchersCount = 0
                      , whRepoLanguage = Nothing
                      , whRepoHasIssues = True
                      , whRepoHasDownloads = True
                      , whRepoHasWiki = True
                      , whRepoHasPages = True
                      , whRepoForkCount = 0
                      , whRepoMirrorUrl = Nothing
                      , whRepoOpenIssuesCount = 1
                      , whRepoDefaultBranchName = "master"
                      }
                , whPullReqTargetLabel = "baxterthehacker:changes"
                , whPullReqTargetRef = "changes"
                }
          , whPullReqMergeableState = Nothing
          }
    , evPullReqRevRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 1
          , whRepoDefaultBranchName = "master"
          }
    , evPullReqRevSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

pullRequestReviewEventFixture :: PullRequestReviewEvent
pullRequestReviewEventFixture = PullRequestReviewEvent
  { evPullReqReviewAction = PullRequestReviewSubmittedAction
  , evPullReqReviewPayload =
      HookPullRequestReview
        { whPullReqReviewId = 2626884
        , whPullReqReviewNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
        , whPullReqReviewUser =
            HookUser
              { whUserLogin = "baxterthehacker"
              , whUserId = 6752317
              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
              , whUserGravatarId = URL ""
              , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
              , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
              , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
              , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
              , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
              , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
              , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
              , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
              , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
              , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
              , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
              , whUserType = OwnerUser
              , whUserIsAdminOfSite = False
              }
        , whPullReqReviewBody = "Looks great!"
        , whPullReqReviewSubmittedAt = read "2016-10-03 23:39:09Z"
        , whPullReqReviewState = "approved"
        , whPullReqReviewHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/8#pullrequestreview-2626884"
        , whPullReqReviewPullUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/8"
        }
  , evPullReqReviewTarget =
      HookPullRequest
        { whPullReqUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/8"
        , whPullReqId = 87811438
        , whPullReqNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
        , whPullReqHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/pull/8"
        , whPullReqDiffUrl = URL "https://github.com/baxterthehacker/public-repo/pull/8.diff"
        , whPullReqPatchUrl = URL "https://github.com/baxterthehacker/public-repo/pull/8.patch"
        , whPullReqIssueUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/8"
        , whPullReqNumber = 8
        , whPullReqState = "open"
        , whPullReqIsLocked = False
        , whPullReqTitle = "Add a README description"
        , whPullReqUser =
            HookUser
              { whUserLogin = "skalnik"
              , whUserId = 2546
              , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
              , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/2546?v=3"
              , whUserGravatarId = URL ""
              , whUserUrl = URL "https://api.github.com/users/skalnik"
              , whUserHtmlUrl = URL "https://github.com/skalnik"
              , whUserFollowersUrl = URL "https://api.github.com/users/skalnik/followers"
              , whUserFollowingUrl = URL "https://api.github.com/users/skalnik/following{/other_user}"
              , whUserGistsUrl = URL "https://api.github.com/users/skalnik/gists{/gist_id}"
              , whUserStarredUrl = URL "https://api.github.com/users/skalnik/starred{/owner}{/repo}"
              , whUserSubscriptionsUrl = URL "https://api.github.com/users/skalnik/subscriptions"
              , whUserOrganizationsUrl = URL "https://api.github.com/users/skalnik/orgs"
              , whUserReposUrl = URL "https://api.github.com/users/skalnik/repos"
              , whUserEventsUrl = URL "https://api.github.com/users/skalnik/events{/privacy}"
              , whUserReceivedEventsUrl = URL "https://api.github.com/users/skalnik/received_events"
              , whUserType = OwnerUser
              , whUserIsAdminOfSite = True
              }
        , whPullReqBody = "Just a few more details"
        , whPullReqCreatedAt = read "2016-10-03 23:37:43Z"
        , whPullReqUpdatedAt = read "2016-10-03 23:39:09Z"
        , whPullReqClosedAt = Nothing
        , whPullReqMergedAt = Nothing
        , whPullReqMergeCommitSha = Just "faea154a7decef6819754aab0f8c0e232e6c8b4f"
        , whPullReqAssignee = Nothing
        , whPullReqMilestone = Nothing
        , whPullReqCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/8/commits"
        , whPullReqRevCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/8/comments"
        , whPullReqRevCommentUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls/comments{/number}"
        , whPullReqCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/8/comments"
        , whPullReqStatusesUrl =
            URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/b7a1f9c27caa4e03c14a88feb56e2d4f7500aa63"
        , whPullReqBase =
            PullRequestTarget
              { whPullReqTargetSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
              , whPullReqTargetUser =
                  HookUser
                    { whUserLogin = "baxterthehacker"
                    , whUserId = 6752317
                    , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                    , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                    , whUserGravatarId = URL ""
                    , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                    , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                    , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                    , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                    , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                    , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                    , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                    , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                    , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                    , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                    , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                    , whUserType = OwnerUser
                    , whUserIsAdminOfSite = False
                    }
              , whPullReqTargetRepo =
                  Just HookRepository
                    { whRepoId = 35129377
                    , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                    , whRepoName = "public-repo"
                    , whRepoFullName = "baxterthehacker/public-repo"
                    , whRepoOwner =
                        Right
                          HookUser
                            { whUserLogin = "baxterthehacker"
                            , whUserId = 6752317
                            , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                            , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                            , whUserGravatarId = URL ""
                            , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                            , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                            , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                            , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                            , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                            , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                            , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                            , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                            , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                            , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                            , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                            , whUserType = OwnerUser
                            , whUserIsAdminOfSite = False
                            }
                    , whRepoIsPrivate = False
                    , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
                    , whRepoDescription = ""
                    , whRepoIsAFork = False
                    , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
                    , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
                    , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
                    , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
                    , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
                    , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
                    , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
                    , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
                    , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
                    , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
                    , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
                    , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
                    , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
                    , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
                    , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
                    , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
                    , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
                    , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
                    , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
                    , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
                    , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
                    , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
                    , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
                    , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
                    , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
                    , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
                    , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
                    , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
                    , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
                    , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
                    , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
                    , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
                    , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
                    , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
                    , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
                    , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
                    , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
                    , whRepoUpdatedAt = read "2016-08-15 17:19:01Z"
                    , whRepoPushedAt = read "2016-10-03 23:37:43Z"
                    , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
                    , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
                    , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
                    , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
                    , whRepoHomepage = Nothing
                    , whRepoSize = 233
                    , whRepoStargazersCount = 2
                    , whRepoWatchersCount = 2
                    , whRepoLanguage = Nothing
                    , whRepoHasIssues = True
                    , whRepoHasDownloads = True
                    , whRepoHasWiki = True
                    , whRepoHasPages = True
                    , whRepoForkCount = 2
                    , whRepoMirrorUrl = Nothing
                    , whRepoOpenIssuesCount = 5
                    , whRepoDefaultBranchName = "master"
                    }
              , whPullReqTargetLabel = "baxterthehacker:master"
              , whPullReqTargetRef = "master"
              }
        , whPullReqHead =
            PullRequestTarget
              { whPullReqTargetSha = "b7a1f9c27caa4e03c14a88feb56e2d4f7500aa63"
              , whPullReqTargetUser =
                  HookUser
                    { whUserLogin = "skalnik"
                    , whUserId = 2546
                    , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                    , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/2546?v=3"
                    , whUserGravatarId = URL ""
                    , whUserUrl = URL "https://api.github.com/users/skalnik"
                    , whUserHtmlUrl = URL "https://github.com/skalnik"
                    , whUserFollowersUrl = URL "https://api.github.com/users/skalnik/followers"
                    , whUserFollowingUrl = URL "https://api.github.com/users/skalnik/following{/other_user}"
                    , whUserGistsUrl = URL "https://api.github.com/users/skalnik/gists{/gist_id}"
                    , whUserStarredUrl = URL "https://api.github.com/users/skalnik/starred{/owner}{/repo}"
                    , whUserSubscriptionsUrl = URL "https://api.github.com/users/skalnik/subscriptions"
                    , whUserOrganizationsUrl = URL "https://api.github.com/users/skalnik/orgs"
                    , whUserReposUrl = URL "https://api.github.com/users/skalnik/repos"
                    , whUserEventsUrl = URL "https://api.github.com/users/skalnik/events{/privacy}"
                    , whUserReceivedEventsUrl = URL "https://api.github.com/users/skalnik/received_events"
                    , whUserType = OwnerUser
                    , whUserIsAdminOfSite = True
                    }
              , whPullReqTargetRepo =
                  Just HookRepository
                    { whRepoId = 69919152
                    , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                    , whRepoName = "public-repo"
                    , whRepoFullName = "skalnik/public-repo"
                    , whRepoOwner =
                        Right
                          HookUser
                            { whUserLogin = "skalnik"
                            , whUserId = 2546
                            , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                            , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/2546?v=3"
                            , whUserGravatarId = URL ""
                            , whUserUrl = URL "https://api.github.com/users/skalnik"
                            , whUserHtmlUrl = URL "https://github.com/skalnik"
                            , whUserFollowersUrl = URL "https://api.github.com/users/skalnik/followers"
                            , whUserFollowingUrl = URL "https://api.github.com/users/skalnik/following{/other_user}"
                            , whUserGistsUrl = URL "https://api.github.com/users/skalnik/gists{/gist_id}"
                            , whUserStarredUrl = URL "https://api.github.com/users/skalnik/starred{/owner}{/repo}"
                            , whUserSubscriptionsUrl = URL "https://api.github.com/users/skalnik/subscriptions"
                            , whUserOrganizationsUrl = URL "https://api.github.com/users/skalnik/orgs"
                            , whUserReposUrl = URL "https://api.github.com/users/skalnik/repos"
                            , whUserEventsUrl = URL "https://api.github.com/users/skalnik/events{/privacy}"
                            , whUserReceivedEventsUrl = URL "https://api.github.com/users/skalnik/received_events"
                            , whUserType = OwnerUser
                            , whUserIsAdminOfSite = True
                            }
                    , whRepoIsPrivate = False
                    , whRepoHtmlUrl = URL "https://github.com/skalnik/public-repo"
                    , whRepoDescription = ""
                    , whRepoIsAFork = True
                    , whRepoUrl = URL "https://api.github.com/repos/skalnik/public-repo"
                    , whRepoForksUrl = URL "https://api.github.com/repos/skalnik/public-repo/forks"
                    , whRepoKeysUrl = URL "https://api.github.com/repos/skalnik/public-repo/keys{/key_id}"
                    , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/skalnik/public-repo/collaborators{/collaborator}"
                    , whRepoTeamsUrl = URL "https://api.github.com/repos/skalnik/public-repo/teams"
                    , whRepoHooksUrl = URL "https://api.github.com/repos/skalnik/public-repo/hooks"
                    , whRepoIssueEventsUrl = URL "https://api.github.com/repos/skalnik/public-repo/issues/events{/number}"
                    , whRepoEventsUrl = URL "https://api.github.com/repos/skalnik/public-repo/events"
                    , whRepoAssigneesUrl = URL "https://api.github.com/repos/skalnik/public-repo/assignees{/user}"
                    , whRepoBranchesUrl = URL "https://api.github.com/repos/skalnik/public-repo/branches{/branch}"
                    , whRepoTagsUrl = URL "https://api.github.com/repos/skalnik/public-repo/tags"
                    , whRepoBlobsUrl = URL "https://api.github.com/repos/skalnik/public-repo/git/blobs{/sha}"
                    , whRepoGitTagsUrl = URL "https://api.github.com/repos/skalnik/public-repo/git/tags{/sha}"
                    , whRepoGitRefsUrl = URL "https://api.github.com/repos/skalnik/public-repo/git/refs{/sha}"
                    , whRepoTreesUrl = URL "https://api.github.com/repos/skalnik/public-repo/git/trees{/sha}"
                    , whRepoStatusesUrl = URL "https://api.github.com/repos/skalnik/public-repo/statuses/{sha}"
                    , whRepoLanguagesUrl = URL "https://api.github.com/repos/skalnik/public-repo/languages"
                    , whRepoStargazersUrl = URL "https://api.github.com/repos/skalnik/public-repo/stargazers"
                    , whRepoContributorsUrl = URL "https://api.github.com/repos/skalnik/public-repo/contributors"
                    , whRepoSubscribersUrl = URL "https://api.github.com/repos/skalnik/public-repo/subscribers"
                    , whRepoSubscriptionUrl = URL "https://api.github.com/repos/skalnik/public-repo/subscription"
                    , whRepoCommitsUrl = URL "https://api.github.com/repos/skalnik/public-repo/commits{/sha}"
                    , whRepoGitCommitsUrl = URL "https://api.github.com/repos/skalnik/public-repo/git/commits{/sha}"
                    , whRepoCommentsUrl = URL "https://api.github.com/repos/skalnik/public-repo/comments{/number}"
                    , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/skalnik/public-repo/issues/comments{/number}"
                    , whRepoContentsUrl = URL "https://api.github.com/repos/skalnik/public-repo/contents/{+path}"
                    , whRepoCompareUrl = URL "https://api.github.com/repos/skalnik/public-repo/compare/{base}...{head}"
                    , whRepoMergesUrl = URL "https://api.github.com/repos/skalnik/public-repo/merges"
                    , whRepoArchiveUrl = URL "https://api.github.com/repos/skalnik/public-repo/{archive_format}{/ref}"
                    , whRepoDownloadsUrl = URL "https://api.github.com/repos/skalnik/public-repo/downloads"
                    , whRepoIssuesUrl = URL "https://api.github.com/repos/skalnik/public-repo/issues{/number}"
                    , whRepoPullsUrl = URL "https://api.github.com/repos/skalnik/public-repo/pulls{/number}"
                    , whRepoMilestonesUrl = URL "https://api.github.com/repos/skalnik/public-repo/milestones{/number}"
                    , whRepoNotificationsUrl = URL "https://api.github.com/repos/skalnik/public-repo/notifications{?since,all,participating}"
                    , whRepoLabelsUrl = URL "https://api.github.com/repos/skalnik/public-repo/labels{/name}"
                    , whRepoReleasesUrl = URL "https://api.github.com/repos/skalnik/public-repo/releases{/id}"
                    , whRepoCreatedAt = read "2016-10-03 23:23:31Z"
                    , whRepoUpdatedAt = read "2016-08-15 17:19:01Z"
                    , whRepoPushedAt = read "2016-10-03 23:36:52Z"
                    , whRepoGitUrl = URL "git://github.com/skalnik/public-repo.git"
                    , whRepoSshUrl = URL "git@github.com:skalnik/public-repo.git"
                    , whRepoCloneUrl = URL "https://github.com/skalnik/public-repo.git"
                    , whRepoSvnUrl = URL "https://github.com/skalnik/public-repo"
                    , whRepoHomepage = Nothing
                    , whRepoSize = 233
                    , whRepoStargazersCount = 0
                    , whRepoWatchersCount = 0
                    , whRepoLanguage = Nothing
                    , whRepoHasIssues = False
                    , whRepoHasDownloads = True
                    , whRepoHasWiki = True
                    , whRepoHasPages = False
                    , whRepoForkCount = 0
                    , whRepoMirrorUrl = Nothing
                    , whRepoOpenIssuesCount = 0
                    , whRepoDefaultBranchName = "master"
                    }
              , whPullReqTargetLabel = "skalnik:patch-2"
              , whPullReqTargetRef = "patch-2"
              }
        , whPullReqMergeableState = Nothing
        , whPullReqCommentCount = Nothing
        , whPullReqRevCommentCount = Nothing
        , whPullReqCommitCount = Nothing
        , whPullReqAdditionsCount = Nothing
        , whPullReqDeletionsCount = Nothing
        , whPullReqFileChangeCount = Nothing
        }
  , evPullReqReviewRepo =
      HookRepository
        { whRepoId = 35129377
        , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
        , whRepoName = "public-repo"
        , whRepoFullName = "baxterthehacker/public-repo"
        , whRepoOwner =
            Right
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
        , whRepoIsPrivate = False
        , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
        , whRepoDescription = ""
        , whRepoIsAFork = False
        , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
        , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
        , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
        , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
        , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
        , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
        , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
        , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
        , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
        , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
        , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
        , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
        , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
        , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
        , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
        , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
        , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
        , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
        , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
        , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
        , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
        , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
        , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
        , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
        , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
        , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
        , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
        , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
        , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
        , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
        , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
        , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
        , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
        , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
        , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
        , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
        , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
        , whRepoUpdatedAt = read "2016-08-15 17:19:01Z"
        , whRepoPushedAt = read "2016-10-03 23:37:43Z"
        , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
        , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
        , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
        , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
        , whRepoHomepage = Nothing
        , whRepoSize = 233
        , whRepoStargazersCount = 2
        , whRepoWatchersCount = 2
        , whRepoLanguage = Nothing
        , whRepoHasIssues = True
        , whRepoHasDownloads = True
        , whRepoHasWiki = True
        , whRepoHasPages = True
        , whRepoForkCount = 2
        , whRepoMirrorUrl = Nothing
        , whRepoOpenIssuesCount = 5
        , whRepoDefaultBranchName = "master"
        }
  , evPullReqReviewSender =
      HookUser
        { whUserLogin = "baxterthehacker"
        , whUserId = 6752317
        , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
        , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
        , whUserGravatarId = URL ""
        , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
        , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
        , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
        , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
        , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
        , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
        , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
        , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
        , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
        , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
        , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
        , whUserType = OwnerUser
        , whUserIsAdminOfSite = False
        }
  }

pushEventFixture :: PushEvent
pushEventFixture = PushEvent
    { evPushRef = "refs/heads/changes"
    , evPushHeadSha = Just "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
    , evPushBeforeSha = Just "9049f1265b7d61be4a8904a9a27120d2064dab3b"
    , evPushCreated = False
    , evPushDeleted = False
    , evPushForced = False
    , evPushBaseRef = Nothing
    , evPushCompareUrl = URL "https://github.com/baxterthehacker/public-repo/compare/9049f1265b7d...0d1a26e67d8f"
    , evPushCommits =
        Just $ V.fromList
          [ HookCommit
              { whCommitSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
              , whCommitUrl = URL "https://github.com/baxterthehacker/public-repo/commit/0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
              , whCommitHtmlUrl = Nothing
              , whCommitCommentsUrl = Nothing
              , whCommitAuthor =
                  Left
                    HookSimpleUser
                      { whSimplUserName = "baxterthehacker"
                      , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                      , whSimplUserLogin = Just "baxterthehacker"
                      }
              , whCommitCommitter =
                  Left
                    HookSimpleUser
                      { whSimplUserName = "baxterthehacker"
                      , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                      , whSimplUserLogin = Just "baxterthehacker"
                      }
              }
          ]
    , evPushHeadCommit =
        Just
          HookCommit
            { whCommitSha = "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
            , whCommitUrl = URL "https://github.com/baxterthehacker/public-repo/commit/0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c"
            , whCommitHtmlUrl = Nothing
            , whCommitCommentsUrl = Nothing
            , whCommitAuthor =
                Left
                  HookSimpleUser
                    { whSimplUserName = "baxterthehacker"
                    , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                    , whSimplUserLogin = Just "baxterthehacker"
                    }
            , whCommitCommitter =
                Left
                  HookSimpleUser
                    { whSimplUserName = "baxterthehacker"
                    , whSimplUserEmail = "baxterthehacker@users.noreply.github.com"
                    , whSimplUserLogin = Just "baxterthehacker"
                    }
            }
    , evPushRepository =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Left
                HookSimpleUser
                  { whSimplUserName = "baxterthehacker" , whSimplUserEmail = "baxterthehacker@users.noreply.github.com" , whSimplUserLogin = Nothing }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:12Z"
          , whRepoPushedAt = read "2015-05-05 23:40:17Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evPushOrganization = Nothing
    , evPushSender =
        Just $ HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

pushEventFixtureWithoutSender :: PushEvent
pushEventFixtureWithoutSender =
  PushEvent
    { evPushRef = "refs/heads/gh-readonly-queue/main/pr-3072-287d30540ac5a1acc57d0bdc138fa81dee315f48",
      evPushHeadSha = Just "0000000000000000000000000000000000000000",
      evPushBeforeSha = Just "a6bd77bbe77ee6ae8f9686621b192d70fcf83e2b",
      evPushCreated = False,
      evPushDeleted = True,
      evPushForced = False,
      evPushBaseRef = Nothing,
      evPushCompareUrl = URL "https://github.com/some-organization/infra-core/compare/a6bd77bbe77e...000000000000",
      evPushCommits = Just V.empty,
      evPushHeadCommit = Nothing,
      evPushRepository =
        HookRepository
          { whRepoId = 353322834,
            whRepoNodeId = "MDEwOJllG9zcXaRcvknzNYyTzM4OUQD=",
            whRepoName = "infra-core",
            whRepoFullName = "some-organization/infra-core",
            whRepoOwner =
              Right
                ( HookUser
                    { whUserLogin = "some-organization",
                      whUserId = 52709322,
                      whUserNodeId = "MDEyO9ky2ZuaXpFdhlGvjbzNzczMTO3Y",
                      whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/52709322?v=4",
                      whUserGravatarId = URL "",
                      whUserUrl = URL "https://api.github.com/users/some-organization",
                      whUserHtmlUrl = URL "https://github.com/some-organization",
                      whUserFollowersUrl = URL "https://api.github.com/users/some-organization/followers",
                      whUserFollowingUrl = URL "https://api.github.com/users/some-organization/following{/other_user}",
                      whUserGistsUrl = URL "https://api.github.com/users/some-organization/gists{/gist_id}",
                      whUserStarredUrl = URL "https://api.github.com/users/some-organization/starred{/owner}{/repo}",
                      whUserSubscriptionsUrl = URL "https://api.github.com/users/some-organization/subscriptions",
                      whUserOrganizationsUrl = URL "https://api.github.com/users/some-organization/orgs",
                      whUserReposUrl = URL "https://api.github.com/users/some-organization/repos",
                      whUserEventsUrl = URL "https://api.github.com/users/some-organization/events{/privacy}",
                      whUserReceivedEventsUrl = URL "https://api.github.com/users/some-organization/received_events",
                      whUserType = OwnerOrganization,
                      whUserIsAdminOfSite = False
                    }
                ),
            whRepoIsPrivate = True,
            whRepoHtmlUrl = URL "https://github.com/some-organization/infra-core",
            whRepoDescription = "Homo sapiens non urinat in ventum",
            whRepoIsAFork = False,
            whRepoUrl = URL "https://github.com/some-organization/infra-core",
            whRepoForksUrl = URL "https://api.github.com/repos/some-organization/infra-core/forks",
            whRepoKeysUrl = URL "https://api.github.com/repos/some-organization/infra-core/keys{/key_id}",
            whRepoCollaboratorsUrl = URL "https://api.github.com/repos/some-organization/infra-core/collaborators{/collaborator}",
            whRepoTeamsUrl = URL "https://api.github.com/repos/some-organization/infra-core/teams",
            whRepoHooksUrl = URL "https://api.github.com/repos/some-organization/infra-core/hooks",
            whRepoIssueEventsUrl = URL "https://api.github.com/repos/some-organization/infra-core/issues/events{/number}",
            whRepoEventsUrl = URL "https://api.github.com/repos/some-organization/infra-core/events",
            whRepoAssigneesUrl = URL "https://api.github.com/repos/some-organization/infra-core/assignees{/user}",
            whRepoBranchesUrl = URL "https://api.github.com/repos/some-organization/infra-core/branches{/branch}",
            whRepoTagsUrl = URL "https://api.github.com/repos/some-organization/infra-core/tags",
            whRepoBlobsUrl = URL "https://api.github.com/repos/some-organization/infra-core/git/blobs{/sha}",
            whRepoGitTagsUrl = URL "https://api.github.com/repos/some-organization/infra-core/git/tags{/sha}",
            whRepoGitRefsUrl = URL "https://api.github.com/repos/some-organization/infra-core/git/refs{/sha}",
            whRepoTreesUrl = URL "https://api.github.com/repos/some-organization/infra-core/git/trees{/sha}",
            whRepoStatusesUrl = URL "https://api.github.com/repos/some-organization/infra-core/statuses/{sha}",
            whRepoLanguagesUrl = URL "https://api.github.com/repos/some-organization/infra-core/languages",
            whRepoStargazersUrl = URL "https://api.github.com/repos/some-organization/infra-core/stargazers",
            whRepoContributorsUrl = URL "https://api.github.com/repos/some-organization/infra-core/contributors",
            whRepoSubscribersUrl = URL "https://api.github.com/repos/some-organization/infra-core/subscribers",
            whRepoSubscriptionUrl = URL "https://api.github.com/repos/some-organization/infra-core/subscription",
            whRepoCommitsUrl = URL "https://api.github.com/repos/some-organization/infra-core/commits{/sha}",
            whRepoGitCommitsUrl = URL "https://api.github.com/repos/some-organization/infra-core/git/commits{/sha}",
            whRepoCommentsUrl = URL "https://api.github.com/repos/some-organization/infra-core/comments{/number}",
            whRepoIssueCommentsUrl = URL "https://api.github.com/repos/some-organization/infra-core/issues/comments{/number}",
            whRepoContentsUrl = URL "https://api.github.com/repos/some-organization/infra-core/contents/{+path}",
            whRepoCompareUrl = URL "https://api.github.com/repos/some-organization/infra-core/compare/{base}...{head}",
            whRepoMergesUrl = URL "https://api.github.com/repos/some-organization/infra-core/merges",
            whRepoArchiveUrl = URL "https://api.github.com/repos/some-organization/infra-core/{archive_format}{/ref}",
            whRepoDownloadsUrl = URL "https://api.github.com/repos/some-organization/infra-core/downloads",
            whRepoIssuesUrl = URL "https://api.github.com/repos/some-organization/infra-core/issues{/number}",
            whRepoPullsUrl = URL "https://api.github.com/repos/some-organization/infra-core/pulls{/number}",
            whRepoMilestonesUrl = URL "https://api.github.com/repos/some-organization/infra-core/milestones{/number}",
            whRepoNotificationsUrl = URL "https://api.github.com/repos/some-organization/infra-core/notifications{?since,all,participating}",
            whRepoLabelsUrl = URL "https://api.github.com/repos/some-organization/infra-core/labels{/name}",
            whRepoReleasesUrl = URL "https://api.github.com/repos/some-organization/infra-core/releases{/id}",
            whRepoCreatedAt = read "2021-04-11 18:30:22Z",
            whRepoUpdatedAt = read "2022-11-07 17:04:28Z",
            whRepoPushedAt = read "2022-12-05 17:41:57Z",
            whRepoGitUrl = URL "git://github.com/some-organization/infra-core.git",
            whRepoSshUrl = URL "git@github.com:some-organization/infra-core.git",
            whRepoCloneUrl = URL "https://github.com/some-organization/infra-core.git",
            whRepoSvnUrl = URL "https://github.com/some-organization/infra-core",
            whRepoHomepage = Just (URL "https://github.com/orgs/some-organization/projects/7"),
            whRepoSize = 23932,
            whRepoStargazersCount = 4,
            whRepoWatchersCount = 4,
            whRepoLanguage = Just "Brainfuck",
            whRepoHasIssues = True,
            whRepoHasDownloads = True,
            whRepoHasWiki = True,
            whRepoHasPages = False,
            whRepoForkCount = 1,
            whRepoMirrorUrl = Nothing,
            whRepoOpenIssuesCount = 130,
            whRepoDefaultBranchName = "main"
          },
      evPushOrganization =
        Just
          ( HookOrganization
              { whOrgLogin = "some-organization",
                whOrgId = 52709322,
                whOrgNodeId = "MDEyO9kZyFua2pXhGlvdjbzNcMzOTYz3",
                whOrgUrl = URL "https://api.github.com/orgs/some-organization",
                whOrgReposUrl = URL "https://api.github.com/orgs/some-organization/repos",
                whOrgEventsUrl = URL "https://api.github.com/orgs/some-organization/events",
                whOrgHooksUrl = Just (URL "https://api.github.com/orgs/some-organization/hooks"),
                whOrgIssuesUrl = Just (URL "https://api.github.com/orgs/some-organization/issues"),
                whOrgMembersUrl = URL "https://api.github.com/orgs/some-organization/members{/member}",
                whOrgPublicMembersUrl = URL "https://api.github.com/orgs/some-organization/public_members{/member}",
                whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/52709322?v=4",
                whOrgDescription = "Plurality Media"
              }
          ),
      evPushSender = Nothing
    }

releaseEventFixture :: ReleaseEvent
releaseEventFixture = ReleaseEvent
    { evReleaseEventAction = ReleasePublishedAction
    , evReleaseEventPayload =
        HookRelease
          { whReleaseUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases/1261438"
          , whReleaseAssetsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases/1261438/assets"
          , whReleaseUploadUrl = URL "https://uploads.github.com/repos/baxterthehacker/public-repo/releases/1261438/assets{?name}"
          , whReleaseHtmlUrl = URL "https://github.com/baxterthehacker/public-repo/releases/tag/0.0.1"
          , whReleaseId = 1261438
          , whReleaseNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whReleaseTagName = "0.0.1"
          , whReleaseTargetCommitish = "master"
          , whReleaseName = Nothing
          , whReleaseIsDraft = False
          , whReleaseAuthor =
              HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whReleaseIsPreRelease = False
          , whReleaseCreatedAt = read "2015-05-05 23:40:12Z"
          , whReleasePublishedAt = Just $ read "2015-05-05 23:40:38Z"
          , whReleaseTarballUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tarball/0.0.1"
          , whReleaseZipballUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/zipball/0.0.1"
          , whReleaseBody = Nothing
          }
    , evReleaseEventRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:38Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evReleaseEventSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

repositoryEventFixture :: RepositoryEvent
repositoryEventFixture = RepositoryEvent
    { evRepositoryAction = RepositoryCreatedAction
    , evRepositoryTarget =
        HookRepository
          { whRepoId = 27496774
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "new-repository"
          , whRepoFullName = "baxterandthehackers/new-repository"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterandthehackers"
                , whUserId = 7649605
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
                , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = True
          , whRepoHtmlUrl = URL "https://github.com/baxterandthehackers/new-repository"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/issues/comments/{number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/milestones{/number}"
          , whRepoNotificationsUrl =
              URL "https://api.github.com/repos/baxterandthehackers/new-repository/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterandthehackers/new-repository/releases{/id}"
          , whRepoCreatedAt = read "2014-12-03 16:39:25Z"
          , whRepoUpdatedAt = read "2014-12-03 16:39:25Z"
          , whRepoPushedAt = read "2014-12-03 16:39:25Z"
          , whRepoGitUrl = URL "git://github.com/baxterandthehackers/new-repository.git"
          , whRepoSshUrl = URL "git@github.com:baxterandthehackers/new-repository.git"
          , whRepoCloneUrl = URL "https://github.com/baxterandthehackers/new-repository.git"
          , whRepoSvnUrl = URL "https://github.com/baxterandthehackers/new-repository"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = False
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evRepositoryOrg =
        Just
          HookOrganization
            { whOrgLogin = "baxterandthehackers"
            , whOrgId = 7649605
            , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
            , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
            , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
            , whOrgHooksUrl = Nothing
            , whOrgIssuesUrl = Nothing
            , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
            , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
            , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=2"
            , whOrgDescription = T.empty
            }
    , evRepositorySender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=2"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

statusEventFixture :: StatusEvent
statusEventFixture = StatusEvent
    { evStatusId = 214015194
    , evStatusCommitSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
    , evStatusCommitName = "baxterthehacker/public-repo"
    , evStatusTargetUrl = Nothing
    , evStatusContext = "default"
    , evStatusDescription = Nothing
    , evStatusState = StatusSuccessState
    , evStatusCommit =
        HookCommit
          { whCommitSha = "9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whCommitUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits/9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whCommitHtmlUrl = Just $ URL "https://github.com/baxterthehacker/public-repo/commit/9049f1265b7d61be4a8904a9a27120d2064dab3b"
          , whCommitCommentsUrl =
              Just $ URL "https://api.github.com/repos/baxterthehacker/public-repo/commits/9049f1265b7d61be4a8904a9a27120d2064dab3b/comments"
          , whCommitAuthor =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whCommitCommitter =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          }
    , evStatusCreatedAt = read "2015-05-05 23:40:39Z"
    , evStatusUpdatedAt = read "2015-05-05 23:40:39Z"
    , evStatusRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:39Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evStatusSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

teamAddEventFixture :: TeamAddEvent
teamAddEventFixture = TeamAddEvent
    { evTeamAddTarget =
        Just
          HookTeam
            { whTeamName = "github"
            , whTeamId = 836012
            , whTeamNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
            , whTeamSlug = "github"
            , whTeamPermission = "pull"
            , whTeamUrl = URL "https://api.github.com/teams/836012"
            , whTeamMembersUrl = URL "https://api.github.com/teams/836012/members{/member}"
            , whTeamRepositoriesUrl = URL "https://api.github.com/teams/836012/repos"
            }
    , evTeamAddRepo =
        HookRepository
          { whRepoId = 35129393
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterandthehackers/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterandthehackers"
                , whUserId = 7649605
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
                , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
                , whUserType = OwnerOrganization
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = True
          , whRepoUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterandthehackers/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:30Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterandthehackers/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterandthehackers/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterandthehackers/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterandthehackers/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = False
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 0
          , whRepoDefaultBranchName = "master"
          }
    , evTeamAddOrg =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 7649605
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Nothing
          , whOrgIssuesUrl = Nothing
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whOrgDescription = T.empty
          }
    , evTeamAddSender =
        HookUser
          { whUserLogin = "baxterandthehackers"
          , whUserId = 7649605
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/7649605?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterandthehackers"
          , whUserHtmlUrl = URL "https://github.com/baxterandthehackers"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterandthehackers/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterandthehackers/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterandthehackers/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterandthehackers/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterandthehackers/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterandthehackers/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterandthehackers/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterandthehackers/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterandthehackers/received_events"
          , whUserType = OwnerOrganization
          , whUserIsAdminOfSite = False
          }
    }

teamEventFixture :: TeamEvent
teamEventFixture = TeamEvent
    { evTeamAction = TeamCreatedAction
    , evTeamTarget =
        HookTeam
          { whTeamName = "team baxter"
          , whTeamId = 2175394
          , whTeamNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whTeamSlug = "team-baxter"
          , whTeamPermission = "pull"
          , whTeamUrl = URL "https:/api.github.com/teams/2175394"
          , whTeamMembersUrl = URL "https:/api.github.com/teams/2175394/members{/member}"
          , whTeamRepositoriesUrl = URL "https:/api.github.com/teams/2175394/repos"
          }
    , evTeamOrganization =
        HookOrganization
          { whOrgLogin = "baxterandthehackers"
          , whOrgId = 4312013
          , whOrgNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whOrgUrl = URL "https://api.github.com/orgs/baxterandthehackers"
          , whOrgReposUrl = URL "https://api.github.com/orgs/baxterandthehackers/repos"
          , whOrgEventsUrl = URL "https://api.github.com/orgs/baxterandthehackers/events"
          , whOrgHooksUrl = Just $ URL "https://api.github.com/orgs/baxterandthehackers/hooks"
          , whOrgIssuesUrl = Just $ URL "https://api.github.com/orgs/baxterandthehackers/issues"
          , whOrgMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/members{/member}"
          , whOrgPublicMembersUrl = URL "https://api.github.com/orgs/baxterandthehackers/public_members{/member}"
          , whOrgAvatarUrl = URL "https://avatars.githubusercontent.com/u/4312013?v=3"
          , whOrgDescription = T.empty
          }
    , evTeamSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }

watchEventFixture :: WatchEvent
watchEventFixture = WatchEvent
    { evWatchAction = WatchStartedAction
    , evWatchRepo =
        HookRepository
          { whRepoId = 35129377
          , whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whRepoName = "public-repo"
          , whRepoFullName = "baxterthehacker/public-repo"
          , whRepoOwner =
              Right HookUser
                { whUserLogin = "baxterthehacker"
                , whUserId = 6752317
                , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
                , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
                , whUserGravatarId = URL ""
                , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
                , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
                , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
                , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
                , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
                , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
                , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
                , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
                , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
                , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
                , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
                , whUserType = OwnerUser
                , whUserIsAdminOfSite = False
                }
          , whRepoIsPrivate = False
          , whRepoHtmlUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoDescription = ""
          , whRepoIsAFork = False
          , whRepoUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo"
          , whRepoForksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/forks"
          , whRepoKeysUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/keys{/key_id}"
          , whRepoCollaboratorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/collaborators{/collaborator}"
          , whRepoTeamsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/teams"
          , whRepoHooksUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/hooks"
          , whRepoIssueEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/events{/number}"
          , whRepoEventsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/events"
          , whRepoAssigneesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/assignees{/user}"
          , whRepoBranchesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/branches{/branch}"
          , whRepoTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/tags"
          , whRepoBlobsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/blobs{/sha}"
          , whRepoGitTagsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/tags{/sha}"
          , whRepoGitRefsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/refs{/sha}"
          , whRepoTreesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/trees{/sha}"
          , whRepoStatusesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/statuses/{sha}"
          , whRepoLanguagesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/languages"
          , whRepoStargazersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/stargazers"
          , whRepoContributorsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contributors"
          , whRepoSubscribersUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscribers"
          , whRepoSubscriptionUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/subscription"
          , whRepoCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/commits{/sha}"
          , whRepoGitCommitsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/git/commits{/sha}"
          , whRepoCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/comments{/number}"
          , whRepoIssueCommentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues/comments{/number}"
          , whRepoContentsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/contents/{+path}"
          , whRepoCompareUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/compare/{base}...{head}"
          , whRepoMergesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/merges"
          , whRepoArchiveUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/{archive_format}{/ref}"
          , whRepoDownloadsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/downloads"
          , whRepoIssuesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/issues{/number}"
          , whRepoPullsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/pulls{/number}"
          , whRepoMilestonesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/milestones{/number}"
          , whRepoNotificationsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/notifications{?since,all,participating}"
          , whRepoLabelsUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/labels{/name}"
          , whRepoReleasesUrl = URL "https://api.github.com/repos/baxterthehacker/public-repo/releases{/id}"
          , whRepoCreatedAt = read "2015-05-05 23:40:12Z"
          , whRepoUpdatedAt = read "2015-05-05 23:40:30Z"
          , whRepoPushedAt = read "2015-05-05 23:40:27Z"
          , whRepoGitUrl = URL "git://github.com/baxterthehacker/public-repo.git"
          , whRepoSshUrl = URL "git@github.com:baxterthehacker/public-repo.git"
          , whRepoCloneUrl = URL "https://github.com/baxterthehacker/public-repo.git"
          , whRepoSvnUrl = URL "https://github.com/baxterthehacker/public-repo"
          , whRepoHomepage = Nothing
          , whRepoSize = 0
          , whRepoStargazersCount = 0
          , whRepoWatchersCount = 0
          , whRepoLanguage = Nothing
          , whRepoHasIssues = True
          , whRepoHasDownloads = True
          , whRepoHasWiki = True
          , whRepoHasPages = True
          , whRepoForkCount = 0
          , whRepoMirrorUrl = Nothing
          , whRepoOpenIssuesCount = 2
          , whRepoDefaultBranchName = "master"
          }
    , evWatchSender =
        HookUser
          { whUserLogin = "baxterthehacker"
          , whUserId = 6752317
          , whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg="
          , whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3"
          , whUserGravatarId = URL ""
          , whUserUrl = URL "https://api.github.com/users/baxterthehacker"
          , whUserHtmlUrl = URL "https://github.com/baxterthehacker"
          , whUserFollowersUrl = URL "https://api.github.com/users/baxterthehacker/followers"
          , whUserFollowingUrl = URL "https://api.github.com/users/baxterthehacker/following{/other_user}"
          , whUserGistsUrl = URL "https://api.github.com/users/baxterthehacker/gists{/gist_id}"
          , whUserStarredUrl = URL "https://api.github.com/users/baxterthehacker/starred{/owner}{/repo}"
          , whUserSubscriptionsUrl = URL "https://api.github.com/users/baxterthehacker/subscriptions"
          , whUserOrganizationsUrl = URL "https://api.github.com/users/baxterthehacker/orgs"
          , whUserReposUrl = URL "https://api.github.com/users/baxterthehacker/repos"
          , whUserEventsUrl = URL "https://api.github.com/users/baxterthehacker/events{/privacy}"
          , whUserReceivedEventsUrl = URL "https://api.github.com/users/baxterthehacker/received_events"
          , whUserType = OwnerUser
          , whUserIsAdminOfSite = False
          }
    }
