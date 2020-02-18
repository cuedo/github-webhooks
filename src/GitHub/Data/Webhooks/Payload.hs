{-# LANGUAGE Trustworthy #-}
{-|
Module      : Github.Data.Webhooks.Payload
Copyright   : (c) ONROCK, 2018
License     : MIT
Maintainer  : Kyle Van Berendonck <foss@onrock.online>

This module contains types that represent GitHub webhook's payload contents.
-}
module GitHub.Data.Webhooks.Payload
    ( -- * Construction Types
      URL(..)
    , getUrl
    , OwnerType(..)
      -- * Webhook Types
    , HookIssue(..)
    , HookRepository(..)
    , HookRepositorySimple(..)
    , HookRepositoryLabel(..)
    , HookUser(..)
    , HookSimpleUser(..)
    , HookOrganization(..)
    , HookOrganizationInvitation(..)
    , HookOrganizationMembership(..)
    , HookTeam(..)
    , HookMilestone(..)
    , HookMembership(..)
    , HookProject(..)
    , HookProjectCard(..)
    , HookProjectColumn(..)
    , HookIssueLabels(..)
    , HookCommit(..)
    , HookCheckSuiteStatus(..)
    , HookCheckSuiteConclusion(..)
    , HookCheckSuite(..)
    , HookCheckSuiteCommit(..)
    , HookCheckRunStatus(..)
    , HookCheckRunConclusion(..)
    , HookCheckRun(..)
    , HookCheckRunOutput(..)
    , HookCheckRunRequestedAction(..)
    , HookChecksInstallation(..)
    , HookChecksPullRequest(..)
    , HookChecksPullRequestRepository(..)
    , HookChecksPullRequestTarget(..)
    , HookRelease(..)
    , HookPullRequest(..)
    , PullRequestTarget(..)
    , HookPullRequestReview(..)
    , HookInstallation(..)
    , HookDeployment(..)
    , HookDeploymentStatus(..)
    , HookWikiPage(..)
    , HookPageBuildResult(..)
    , HookIssueComment(..)
    , HookCommitComment(..)
    , HookPullRequestReviewComment(..)
    ) where

import           Data.Aeson               (FromJSON(..), withObject, withText, (.!=), (.:), (.:?))
import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Applicative      ((<|>), (<*>), pure)
import           Data.Data                (Data, Typeable)
import           Data.Functor             ((<$>))
import           Data.Time                (UTCTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Vector              (Vector)
import           GHC.Generics             (Generic)


-- Types lifted from the @github@ package.

-- | Represents the owner of a repository, pull request or similar.
--
-- A bot is a "special type of user which takes actions on behalf of GitHub Apps".
-- See also https://developer.github.com/v4/object/bot/
data OwnerType = OwnerUser | OwnerOrganization | OwnerBot
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable, Data)

instance NFData OwnerType

instance FromJSON OwnerType where
  parseJSON = withText "Owner type" $ \t ->
      case T.toLower t of
          "user"          -> pure OwnerUser
          "organization"  -> pure OwnerOrganization
          "bot"           -> pure OwnerBot
          _               -> fail $ "Unknown owner type: " ++ T.unpack t


-- | Represents an internet address that would be suitable to query
-- for more information. The GitHub API only returns valid 'URL's.
newtype URL = URL Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData URL where rnf = genericRnf

instance FromJSON URL where
  parseJSON = withText "URL" (pure . URL)

-- | Demote GitHub URL to Text.
getUrl :: URL -> Text
getUrl (URL url) = url


-- Hooks

type IssueState = Text

-- | Represents the "issue" field in the 'IssueCommentEvent'
-- and 'IssueEvent' payload.
data HookIssue = HookIssue
    { whIssueUrl                :: !URL
    , whIssueLabelsUrl          :: !URL
    , whIssueCommentsUrl        :: !URL
    , whIssueEventsUrl          :: !URL
    , whIssueHtmlUrl            :: !URL
    , whIssueId                 :: !Int
    , whIssueNumber             :: !Int
    , whIssueTitle              :: !Text
    , whIssueUser               :: !HookUser
    , whIssueLabels             :: !(Vector HookIssueLabels)
    , whIssueState              :: IssueState
    , whIssueIsLocked           :: !Bool
    , whIssueAssignee           :: !(Maybe HookUser)
    , whIssueMilestone          :: !(Maybe HookMilestone)
    , whIssueCommentCount       :: !Int
    , whIssueCreatedAt          :: !UTCTime
    , whIssueUpdatedAt          :: !UTCTime
    , whIssueClosedAt           :: !(Maybe UTCTime)
    , whIssueBody               :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssue where rnf = genericRnf

-- | Represents the "repository" field in all types of payload.
data HookRepository = HookRepository
    { whRepoId                  :: !Int
    , whRepoName                :: !Text
    , whRepoFullName            :: !Text
    , whRepoOwner               :: !(Either HookSimpleUser HookUser)
    , whRepoIsPrivate           :: !Bool
    , whRepoHtmlUrl             :: !URL
    , whRepoDescription         :: !Text
    , whRepoIsAFork             :: !Bool
    , whRepoUrl                 :: !URL
    , whRepoForksUrl            :: !URL
    , whRepoKeysUrl             :: !URL
    , whRepoCollaboratorsUrl    :: !URL
    , whRepoTeamsUrl            :: !URL
    , whRepoHooksUrl            :: !URL
    , whRepoIssueEventsUrl      :: !URL
    , whRepoEventsUrl           :: !URL
    , whRepoAssigneesUrl        :: !URL
    , whRepoBranchesUrl         :: !URL
    , whRepoTagsUrl             :: !URL
    , whRepoBlobsUrl            :: !URL
    , whRepoGitTagsUrl          :: !URL
    , whRepoGitRefsUrl          :: !URL
    , whRepoTreesUrl            :: !URL
    , whRepoStatusesUrl         :: !URL
    , whRepoLanguagesUrl        :: !URL
    , whRepoStargazersUrl       :: !URL
    , whRepoContributorsUrl     :: !URL
    , whRepoSubscribersUrl      :: !URL
    , whRepoSubscriptionUrl     :: !URL
    , whRepoCommitsUrl          :: !URL
    , whRepoGitCommitsUrl       :: !URL
    , whRepoCommentsUrl         :: !URL
    , whRepoIssueCommentsUrl    :: !URL
    , whRepoContentsUrl         :: !URL
    , whRepoCompareUrl          :: !URL
    , whRepoMergesUrl           :: !URL
    , whRepoArchiveUrl          :: !URL
    , whRepoDownloadsUrl        :: !URL
    , whRepoIssuesUrl           :: !URL
    , whRepoPullsUrl            :: !URL
    , whRepoMilestonesUrl       :: !URL
    , whRepoNotificationsUrl    :: !URL
    , whRepoLabelsUrl           :: !URL
    , whRepoReleasesUrl         :: !URL
    , whRepoCreatedAt           :: !UTCTime
    , whRepoUpdatedAt           :: !UTCTime
    , whRepoPushedAt            :: !UTCTime
    , whRepoGitUrl              :: !URL
    , whRepoSshUrl              :: !URL
    , whRepoCloneUrl            :: !URL
    , whRepoSvnUrl              :: !URL
    , whRepoHomepage            :: !(Maybe URL)
    , whRepoSize                :: !Int
    , whRepoStargazersCount     :: !Int
    , whRepoWatchersCount       :: !Int
    , whRepoLanguage            :: !(Maybe Text)
    , whRepoHasIssues           :: !Bool
    , whRepoHasDownloads        :: !Bool
    , whRepoHasWiki             :: !Bool
    , whRepoHasPages            :: !Bool
    , whRepoForkCount           :: !Int
    , whRepoMirrorUrl           :: !(Maybe URL)
    , whRepoOpenIssuesCount     :: !Int
    , whRepoDefaultBranchName   :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRepository where rnf = genericRnf

-- | Represents the "repositories_added" and "repositories_removed"
-- field in the 'InstallationRepositoriesEvent' payload.
data HookRepositorySimple = HookRepositorySimple
  { whSimplRepoId               :: !Int
  , whSimplRepoName             :: !Text
  , whSimplRepoFullName         :: !Text
  , whSimplRepoIsPrivate        :: !Bool
  }
  deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRepositorySimple where rnf = genericRnf

-- | Represents the "label" field in the 'LabelEvent' payload.
data HookRepositoryLabel = HookRepositoryLabel
    { whRepoLabelUrl            :: !URL
    , whRepoLabelName           :: !Text
    , whRepoLabelColor          :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRepositoryLabel where rnf = genericRnf

-- | Represents the "user" field in all types of payload.
data HookUser = HookUser
    { whUserLogin               :: !Text
    , whUserId                  :: !Int
    , whUserAvatarUrl           :: !URL
    , whUserGravatarId          :: !URL
    , whUserUrl                 :: !URL
    , whUserHtmlUrl             :: !URL
    , whUserFollowersUrl        :: !URL
    , whUserFollowingUrl        :: !URL
    , whUserGistsUrl            :: !URL
    , whUserStarredUrl          :: !URL
    , whUserSubscriptionsUrl    :: !URL
    , whUserOrganizationsUrl    :: !URL
    , whUserReposUrl            :: !URL
    , whUserEventsUrl           :: !URL
    , whUserReceivedEventsUrl   :: !URL
    , whUserType                :: !OwnerType
    , whUserIsAdminOfSite       :: !Bool
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookUser where rnf = genericRnf

-- FIXME: Not sure where this is.
data HookSimpleUser = HookSimpleUser
    { whSimplUserName           :: !Text
    , whSimplUserEmail          :: !Text
    , whSimplUserLogin          :: !(Maybe Text)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookSimpleUser where rnf = genericRnf

-- | Represents the "organization" field in all types of payload.
data HookOrganization = HookOrganization
    { whOrgLogin                :: !Text
    , whOrgId                   :: !Int
    , whOrgUrl                  :: !URL
    , whOrgReposUrl             :: !URL
    , whOrgEventsUrl            :: !URL
    , whOrgHooksUrl             :: !(Maybe URL)
    , whOrgIssuesUrl            :: !(Maybe URL)
    , whOrgMembersUrl           :: !URL
    , whOrgPublicMembersUrl     :: !URL
    , whOrgAvatarUrl            :: !URL
    , whOrgDescription          :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganization where rnf = genericRnf

-- | Represents the "invitation" field in the 'OrganizationEvent' payload.
data HookOrganizationInvitation = HookOrganizationInvitation
    { whOrgInvitationId         :: !Int
    , whOrgInvitationLogin      :: !Text
    , whOrgInvitationEmail      :: !(Maybe Text)
    , whOrgInvitationRole       :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganizationInvitation where rnf = genericRnf

-- | Represents the "membership" field in the 'OrganizationEvent' payload.
data HookOrganizationMembership = HookOrganizationMembership
    { whOrgMembershipUrl        :: !URL
    , whOrgMembershipState      :: !Text
    , whOrgMembershipRole       :: !Text
    , whOrgMembershipOrgUrl     :: !URL
    , whOrgMembershipUser       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganizationMembership where rnf = genericRnf

-- | Represents the "team" field in the 'TeamEvent' and
-- 'TeamAddEvent' payload.
data HookTeam = HookTeam
    { whTeamName                :: !Text
    , whTeamId                  :: !Int
    , whTeamSlug                :: !Text
    , whTeamPermission          :: !Text
    , whTeamUrl                 :: !URL
    , whTeamMembersUrl          :: !URL
    , whTeamRepositoriesUrl     :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookTeam where rnf = genericRnf


type MilestoneState = Text

-- | Represents the "milestone" field in the 'MilestoneEvent' payload.
data HookMilestone = HookMilestone
    { whMilestoneUrl            :: !URL
    , whMilestoneHtmlUrl        :: !URL
    , whMilestoneLabelsUrl      :: !URL
    , whMilestoneId             :: !Int
    , whMilestoneNumber         :: !Int
    , whMilestoneTitle          :: !Text
    , whMilestoneDescription    :: !(Maybe Text)
    , whMilestoneCreator        :: !HookUser
    , whMilestoneOpenIssues     :: !Int
    , whMilestoneClosedIssues   :: !Int
    , whMilestoneState          :: !MilestoneState
    , whMilestoneCreatedAt      :: !UTCTime
    , whMilestoneUpdatedAt      :: !UTCTime
    , whMilestoneDueOn          :: !(Maybe UTCTime)
    , whMilestoneClosedAt       :: !(Maybe UTCTime)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookMilestone where rnf = genericRnf


type MembershipState = Text
type MembershipRole = Text

-- FIXME: Not sure where this is.
data HookMembership = HookMembership
    { whMembershipUrl           :: !URL
    , whMembershipState         :: !MembershipState
    , whMembershipRole          :: !MembershipRole
    , whMembershipOrgUrl        :: !URL
    , whMembershipUser          :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookMembership where rnf = genericRnf


type ProjectState = Text

-- | Represents the "project" field in the 'ProjectEvent' payload.
data HookProject = HookProject
    { whProjectOwnerUrl         :: !URL
    , whProjectUrl              :: !URL
    , whProjectColumnsUrl       :: !URL
    , whProjectId               :: !Int
    , whProjectName             :: !Text
    , whProjectBody             :: !Text
    , whProjectNumber           :: !Int
    , whProjectState            :: !ProjectState
    , whProjectCreator          :: !HookUser
    , whProjectCreatedAt        :: !UTCTime
    , whProjectUpdatedAt        :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProject where rnf = genericRnf

-- | Represents the "project_card" field in the 'ProjectCardEvent' payload.
data HookProjectCard = HookProjectCard
    { whProjectCardUrl          :: !URL
    , whProjectCardColumnUrl    :: !URL
    , whProjectCardColumnId     :: !Int
    , whProjectCardId           :: !Int
    , whProjectCardNote         :: !(Maybe Text)
    , whProjectCardCreator      :: !HookUser
    , whProjectCardCreatedAt    :: !UTCTime
    , whProjectCardUpdatedAt    :: !UTCTime
    , whProjectCardContentUrl   :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProjectCard where rnf = genericRnf

-- | Represents the "project_column" field in the 'ProjectColumnEvent' payload.
data HookProjectColumn = HookProjectColumn
    { whProjectColumnUrl        :: !URL
    , whProjectColumnProjUrl    :: !URL
    , whProjectColumnCardsUrl   :: !URL
    , whProjectColumnId         :: !Int
    , whProjectColumnName       :: !Text
    , whProjectColumnCreatedAt  :: !UTCTime
    , whProjectColumnUpdatedAt  :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProjectColumn where rnf = genericRnf

-- | Represents the "issue.labels" field in the
-- 'IssueCommentEvent' and 'IssueEvent' payloads.
data HookIssueLabels = HookIssueLabels
    { whIssueLabelId            :: !(Maybe Int)   -- ^ Not always sent.
    , whIssueLabelUrl           :: !URL
    , whIssueLabelName          :: !Text
    , whIssueLabelColor         :: !Text
    , whIssueLabelIsDefault     :: !Bool          -- ^ Defaults to false when not present.
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssueLabels where rnf = genericRnf

-- | Represents the "status" field in the
-- 'HookCheckSuite' payload.
data HookCheckSuiteStatus
    -- | Decodes from "requested"
    = HookCheckSuiteStatusRequested
    -- | Decodes from "queued".
    | HookCheckSuiteStatusQueued
    -- | Decodes from "in_progress"
    | HookCheckSuiteStatusInProgress
    -- | Decodes from "completed"
    | HookCheckSuiteStatusCompleted
    -- | The result of decoding an unknown check suite status type
    | HookCheckSuiteStatusOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData HookCheckSuiteStatus where rnf = genericRnf

instance FromJSON HookCheckSuiteStatus where
  parseJSON = withText "Hook check suite status" $ \t ->
      case t of
          "requested"          -> pure HookCheckSuiteStatusRequested
          "queued"             -> pure HookCheckSuiteStatusQueued
          "in_progress"        -> pure HookCheckSuiteStatusInProgress
          "completed"          -> pure HookCheckSuiteStatusCompleted
          _                    -> pure (HookCheckSuiteStatusOther t)

-- | Represents the "conclusion" field in the
-- 'HookCheckSuite' payload.
data HookCheckSuiteConclusion
    -- | Decodes from "success"
    = HookCheckSuiteConclusionSuccess
    -- | Decodes from "failure"
    | HookCheckSuiteConclusionFailure
    -- | Decodes from "neutral"
    | HookCheckSuiteConclusionNeutral
    -- | Decodes from "cancelled"
    | HookCheckSuiteConclusionCancelled
    -- | Decodes from "timed_out"
    | HookCheckSuiteConclusionTimedOut
    -- | Decodes from "action_required"
    | HookCheckSuiteConclusionActionRequired
    -- | Decodes from "stale"
    | HookCheckSuiteConclusionStale
    -- | The result of decoding an unknown check suite conclusion type
    | HookCheckSuiteConclusionOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData HookCheckSuiteConclusion where rnf = genericRnf

instance FromJSON HookCheckSuiteConclusion where
  parseJSON = withText "Hook check suite status" $ \t ->
      case t of
          "success"               -> pure HookCheckSuiteConclusionSuccess
          "failure"               -> pure HookCheckSuiteConclusionFailure
          "neutral"               -> pure HookCheckSuiteConclusionNeutral
          "cancelled"             -> pure HookCheckSuiteConclusionCancelled
          "timed_out"             -> pure HookCheckSuiteConclusionTimedOut
          "action_required"       -> pure HookCheckSuiteConclusionActionRequired
          "stale"                 -> pure HookCheckSuiteConclusionStale
          _                       -> pure (HookCheckSuiteConclusionOther t)

-- FIXME: Missing nested "app", there are examples, but no documentation.
-- | Represents the "check_suite" field in the
-- 'CheckSuiteEvent' payload.
data HookCheckSuite = HookCheckSuite
    { whCheckSuiteId                   :: !Int
    , whCheckSuiteHeadBranch           :: !(Maybe Text) -- ^ The Checks API only looks for pushes in the repository where the check suite or check run were created. Pushes to a branch in a forked repository are not detected and return an empty pull_requests array and a null value for head_branch.
    , whCheckSuiteHeadSha              :: !Text
    , whCheckSuiteStatus               :: !HookCheckSuiteStatus
    , whCheckSuiteConclusion           :: !(Maybe HookCheckSuiteConclusion)
    , whCheckSuiteUrl                  :: !URL
    , whCheckSuiteBeforeSha            :: !(Maybe Text)
    , whCheckSuiteAfterSha             :: !Text
    , whCheckSuitePullRequests         :: !(Vector HookChecksPullRequest)
    , whCheckSuiteCreatedAt            :: !UTCTime
    , whCheckSuiteUpdatedAt            :: !UTCTime
    , whCheckSuiteLatestCheckRunsCount :: !(Maybe Int) -- not included in the check run nested payload
    , whCheckSuiteCheckRunsUrl         :: !(Maybe URL) -- not included in the check run nested payload
    , whCheckSuiteHeadCommit           :: !(Maybe HookCheckSuiteCommit) -- not included in the check run nested payload
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckSuite where rnf = genericRnf

-- | Represents the "head_commit" field in the
--  'CheckSuiteEvent' payload.
data HookCheckSuiteCommit = HookCheckSuiteCommit
    { whCheckSuiteCommitSha               :: !Text          -- ^ Sometimes called the commit 'id'.
    , whCheckSuiteCommitAuthor            :: !HookSimpleUser
    , whCheckSuiteCommitCommitter         :: !HookSimpleUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckSuiteCommit where rnf = genericRnf

-- | Represents the "status" field in the
--  'HookCheckRun' payload.
data HookCheckRunStatus
    -- | Decodes from "queued"
    = HookCheckRunStatusQueued
    -- | Decodes from "in_progress"
    | HookCheckRunStatusInProgress
    -- | Decodes from "completed"
    | HookCheckRunStatusCompleted
    -- | The result of decoding an unknown check run status type
    | HookCheckRunStatusOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData HookCheckRunStatus where rnf = genericRnf

instance FromJSON HookCheckRunStatus where
  parseJSON = withText "Hook check suite status" $ \t ->
      case t of
          "queued"             -> pure HookCheckRunStatusQueued
          "in_progress"        -> pure HookCheckRunStatusInProgress
          "completed"          -> pure HookCheckRunStatusCompleted
          _                    -> pure (HookCheckRunStatusOther t)

-- | Represents the "conclusion" field in the
--  'HookCheckRun' payload.
data HookCheckRunConclusion
    -- | Decodes from "success"
    = HookCheckRunConclusionSuccess
    -- | Decodes from "failure"
    | HookCheckRunConclusionFailure
    -- | Decodes from "neutral"
    | HookCheckRunConclusionNeutral
    -- | Decodes from "cancelled"
    | HookCheckRunConclusionCancelled
    -- | Decodes from "timed_out"
    | HookCheckRunConclusionTimedOut
    -- | Decodes from "action_required"
    | HookCheckRunConclusionActionRequired
    -- | Decodes from "stale"
    | HookCheckRunConclusionStale
    -- | The result of decoding an unknown check run conclusion type
    | HookCheckRunConclusionOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData HookCheckRunConclusion where rnf = genericRnf

instance FromJSON HookCheckRunConclusion where
  parseJSON = withText "Hook check suite status" $ \t ->
      case t of
          "success"               -> pure HookCheckRunConclusionSuccess
          "failure"               -> pure HookCheckRunConclusionFailure
          "neutral"               -> pure HookCheckRunConclusionNeutral
          "cancelled"             -> pure HookCheckRunConclusionCancelled
          "timed_out"             -> pure HookCheckRunConclusionTimedOut
          "action_required"       -> pure HookCheckRunConclusionActionRequired
          "stale"                 -> pure HookCheckRunConclusionStale
          _                       -> pure (HookCheckRunConclusionOther t)

-- FIXME: Missing nested "app", there are examples, but no documentation.
-- | Represents the "check_run" field in the
--  'CheckRunEvent' payload.
data HookCheckRun = HookCheckRun
    { whCheckRunId                   :: !Int
    , whCheckRunHeadSha              :: !Text
    , whCheckRunExternalId           :: !Text
    , whCheckRunUrl                  :: !URL
    , whCheckRunHtmlUrl              :: !URL
    , whCheckRunDetailsUrl           :: !URL
    , whCheckRunStatus               :: !HookCheckRunStatus
    , whCheckRunConclusion           :: !(Maybe HookCheckRunConclusion)
    , whCheckRunStartedAt            :: !UTCTime
    , whCheckRunCompletedAt          :: !(Maybe UTCTime)
    , whCheckRunOutput               :: !HookCheckRunOutput
    , whCheckRunName                 :: !Text
    , weCheckRunCheckSuite           :: !HookCheckSuite
    , whCheckRunPullRequests         :: !(Vector HookChecksPullRequest)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckRun where rnf = genericRnf

-- | Represents the "output" field in the
--  'HookCheckRun' payload.
data HookCheckRunOutput = HookCheckRunOutput
    { whCheckRunOutputTitle            :: !(Maybe Text)
    , whCheckRunOutputSummary          :: !(Maybe Text)
    , whCheckRunOutputText             :: !(Maybe Text)
    , whCheckRunOutputAnnotationsCount :: !Int
    , whCheckRunOutputAnnotationsUrl   :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckRunOutput where rnf = genericRnf

-- | Represents the "requested_action" field in the
--  'CheckRunEvent' payload.
newtype HookCheckRunRequestedAction = HookCheckRunRequestedAction
    { whCheckRunRequestedActionIdentifier       :: Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCheckRunRequestedAction where rnf = genericRnf

-- | Represents the "installation" field in the checks payloads.
newtype HookChecksInstallation = HookChecksInstallation
    { whChecksInstallationId    :: Int
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookChecksInstallation where rnf = genericRnf

-- | Represents the "pull_requests" field in the checks payloads.
data HookChecksPullRequest = HookChecksPullRequest
    { whChecksPullRequestUrl              :: !URL
    , whChecksPullRequestId               :: !Int
    , whChecksPullRequestNumber           :: !Int
    , whChecksPullRequestHead             :: !HookChecksPullRequestTarget
    , whChecksPullRequestBase             :: !HookChecksPullRequestTarget
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookChecksPullRequest where rnf = genericRnf

-- | Represents the "repo" field in the checks pull_request payloads.
data HookChecksPullRequestRepository = HookChecksPullRequestRepository
    { whChecksPullRequestRepositoryId                  :: !Int
    , whChecksPullRequestRepositoryUrl                 :: !URL
    , whChecksPullRequestRepositoryName                :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookChecksPullRequestRepository where rnf = genericRnf

-- | Represents the repo targets in
--  the checks pull requests repository payloads.
data  HookChecksPullRequestTarget = HookChecksPullRequestTarget
    { whChecksPullRequestTargetSha  :: !Text
    , whChecksPullRequestTargetRef  :: !Text -- ex "somebranch"
    , whChecksPullRequestTargetRepo :: !HookChecksPullRequestRepository
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookChecksPullRequestTarget where rnf = genericRnf

--- FIXME: Missing nested metadata that provides commit description
--- FIXME: Missing property "parent" (no examples provided)
data HookCommit = HookCommit
    { whCommitSha               :: !Text          -- ^ Sometimes called the commit 'id'.
    , whCommitUrl               :: !URL
    , whCommitHtmlUrl           :: !(Maybe URL)   -- ^ Not always sent.
    , whCommitCommentsUrl       :: !(Maybe URL)   -- ^ Not always sent.
    , whCommitAuthor            :: !(Either HookSimpleUser HookUser)
    , whCommitCommitter         :: !(Either HookSimpleUser HookUser)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCommit where rnf = genericRnf

-- FIXME: Missing property "assets" (no examples provided)
data HookRelease = HookRelease
    { whReleaseUrl              :: !URL
    , whReleaseAssetsUrl        :: !URL
    , whReleaseUploadUrl        :: !URL
    , whReleaseHtmlUrl          :: !URL
    , whReleaseId               :: !Int
    , whReleaseTagName          :: !Text
    , whReleaseTargetCommitish  :: !Text
    , whReleaseName             :: !(Maybe Text)
    , whReleaseIsDraft          :: !Bool
    , whReleaseAuthor           :: !HookUser
    , whReleaseIsPreRelease     :: !Bool
    , whReleaseCreatedAt        :: !UTCTime
    , whReleasePublishedAt      :: !(Maybe UTCTime)
    , whReleaseTarballUrl       :: !URL
    , whReleaseZipballUrl       :: !URL
    , whReleaseBody             :: !(Maybe Text)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRelease where rnf = genericRnf

data HookPullRequest = HookPullRequest
    { whPullReqUrl              :: !URL
    , whPullReqId               :: !Int
    , whPullReqHtmlUrl          :: !URL
    , whPullReqDiffUrl          :: !URL
    , whPullReqPatchUrl         :: !URL
    , whPullReqIssueUrl         :: !URL
    , whPullReqNumber           :: !Int
    , whPullReqState            :: !Text -- FIXME: Smart constructor?
    , whPullReqIsLocked         :: !Bool
    , whPullReqTitle            :: !Text
    , whPullReqUser             :: !HookUser
    , whPullReqBody             :: !Text
    , whPullReqCreatedAt        :: !UTCTime
    , whPullReqUpdatedAt        :: !UTCTime
    , whPullReqClosedAt         :: !(Maybe UTCTime)
    , whPullReqMergedAt         :: !(Maybe UTCTime)
    , whPullReqMergeCommitSha   :: !(Maybe Text)
    , whPullReqAssignee         :: !(Maybe HookUser)
    , whPullReqMilestone        :: !(Maybe HookMilestone)
    , whPullReqCommitsUrl       :: !URL
    , whPullReqRevCommentsUrl   :: !URL
    , whPullReqRevCommentUrl    :: !URL
    , whPullReqCommentsUrl      :: !URL
    , whPullReqStatusesUrl      :: !URL
    , whPullReqBase             :: !PullRequestTarget
    , whPullReqHead             :: !PullRequestTarget
    -- , whPullReqIsMerged         :: !Bool
    -- , whPullReqIsMergeable      :: !Bool
    -- , whPullReqMergeableState   :: !Text
    -- , whPullReqMergedBy         :: !(Maybe HookUser)
    , whPullReqCommentCount     :: !(Maybe Int)               -- ^ Not sent with all events.
    , whPullReqRevCommentCount  :: !(Maybe Int)               -- ^ Not sent with all events.
    , whPullReqCommitCount      :: !(Maybe Int)               -- ^ Not sent with all events.
    , whPullReqAdditionsCount   :: !(Maybe Int)               -- ^ Not sent with all events.
    , whPullReqDeletionsCount   :: !(Maybe Int)               -- ^ Not sent with all events.
    , whPullReqFileChangeCount  :: !(Maybe Int)               -- ^ Not sent with all events.
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequest where rnf = genericRnf

data PullRequestTarget = PullRequestTarget
    { whPullReqTargetSha :: !Text
    , whPullReqTargetUser :: !HookUser
    , whPullReqTargetRepo :: !HookRepository
    , whPullReqTargetLabel :: !Text -- ex "user:branch"
    , whPullReqTargetRef :: !Text -- ex "somebranch"
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData PullRequestTarget where rnf = genericRnf

-- | Represents the "pull_request" field in the 'PullRequestReviewEvent' payload.
data HookPullRequestReview = HookPullRequestReview
    { whPullReqReviewId         :: !Int
    , whPullReqReviewUser       :: !HookUser
    , whPullReqReviewBody       :: !Text
    , whPullReqReviewSubmittedAt :: !UTCTime
    , whPullReqReviewState      :: !Text
    , whPullReqReviewHtmlUrl    :: !URL
    , whPullReqReviewPullUrl    :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequestReview where rnf = genericRnf

-- | Represents the "installation" field in the 'InstallationEvent' payload.
data HookInstallation = HookInstallation
    { whInstallationId          :: !Int
    , whInstallationAccount     :: !HookUser
    , whInstallationRepoSel     :: !Text
    , whInstallationTokenUrl    :: !URL
    , whInstallationRepoUrl     :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookInstallation where rnf = genericRnf

-- | Represents the "deployment" field in the
-- 'DeploymentEvent' and 'DeploymentStatusEvent' payload.
data HookDeployment = HookDeployment
    { whDeploymentUrl           :: !URL
    , whDeploymentId            :: !Int
    , whDeploymentSha           :: !Text
    , whDeploymentRef           :: !Text
    , whDeploymentTask          :: !Text
    -- , whDeploymentPayload
    , whDeploymentEnv           :: !Text
    , whDeploymentDescription   :: !(Maybe Text)
    , whDeploymentCreator       :: !HookUser
    , whDeploymentCreatedAt     :: !UTCTime
    , whDeploymentUpdatedAt     :: !UTCTime
    , whDeploymentStatusesUrl   :: !URL
    , whDeploymentRepoUrl       :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookDeployment where rnf = genericRnf

-- | Represents the "deployment_status" field in the
-- 'DeploymentStatusEvent' payload.
data HookDeploymentStatus = HookDeploymentStatus
    { whDeploymentStatusUrl     :: !URL
    , whDeploymentStatusId      :: !Int
    , whDeploymentStatusState   :: !Text
    , whDeploymentStatusCreator :: !HookUser
    , whDeploymentStatusDesc    :: !(Maybe Text)
    , whDeploymentStatusTargetUrl :: !(Maybe URL)
    , whDeploymentStatusCreatedAt :: !UTCTime
    , whDeploymentStatusUpdatedAt :: !UTCTime
    , whDeploymentStatusDeplUrl   :: !URL
    , whDeploymentStatusRepoUrl   :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookDeploymentStatus where rnf = genericRnf

-- | Represents the "pages" field in the 'GollumEvent' payload.
data HookWikiPage = HookWikiPage
    { whWikiPageName            :: !Text
    , whWikiPageTitle           :: !Text
    , whWikiPageSummary         :: !(Maybe Text)
    , wkWikiPageAction          :: !Text
    , whWikiPageSha             :: !Text
    , whWikiPageHtmlUrl         :: URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookWikiPage where rnf = genericRnf

-- | Represents the "build" field in the 'PageBuildEvent' payload.
data HookPageBuildResult = HookPageBuildResult
    { whPageBuildUrl            :: !URL
    , whPageBuildStatus         :: !Text
    , whPageBuildError          :: !(Maybe Text)
    , whPageBuildPusher         :: !HookUser
    , whPageBuildCommitSha      :: !Text
    , whPageBuildDuration       :: !Int
    , whPageBuildCreatedAt      :: !UTCTime
    , whPageBuildUpdatedAt      :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPageBuildResult where rnf = genericRnf

-- | Represents the "issue" field in 'IssueComentEvent' payload.
data HookIssueComment = HookIssueComment
    { whIssueCommentUrl         :: !URL
    , whIssueCommentHtmlUrl     :: !URL
    , whIssueCommentIssueUrl    :: !URL
    , whIssueCommentId          :: !Int
    , whIssueCommentUser        :: !HookUser
    , whIssueCommentCreatedAt   :: !UTCTime
    , whIssueCommentUpdatedAt   :: !UTCTime
    , whIssueCommentBody        :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssueComment where rnf = genericRnf

-- | Represents the "comment" field in the 'CommitCommentEvent' payload.
data HookCommitComment = HookCommitComment
    { whCommitCommentUrl        :: !URL
    , whCommitCommentHtmlUrl    :: !URL
    , whCommitCommentId         :: !Int
    , whCommitCommentUser       :: !HookUser
    , whCommitCommentPos        :: !(Maybe Int)
    , whCommitCommentLine       :: !(Maybe Int)
    , whCommitCommentPath       :: !(Maybe Text)
    , whCommitCommentCommitSha  :: !Text
    , whCommitCommentCreatedAt  :: !UTCTime
    , whCommitCommentUpdatedAt  :: !UTCTime
    , whCommitCommentBody       :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCommitComment where rnf = genericRnf

-- | Represents the "pull_request" field in the
-- 'PullRequestReviewCommentEvent' payload.
data HookPullRequestReviewComment = HookPullRequestReviewComment
    { whPullReqRevComUrl        :: !URL
    , whPullReqRevComId         :: !Int
    , whPullReqRevComDiffHunk   :: !Text
    , whPullReqRevComPath       :: !Text
    , whPullReqRevComPos        :: !Int
    , whPullReqRevComOrigPos    :: !Int
    , whPullReqRevComCommitSha  :: !Text
    , whPullReqRevComOrigSha    :: !Text
    , whPullReqRevComUser       :: !HookUser
    , whPullReqRevComBody       :: !Text
    , whPullReqRevComCreatedAt  :: !UTCTime
    , whPullReqRevComUpdatedAt  :: !UTCTime
    , whPullReqRevComHtmlUrl    :: !URL
    , whPullReqRevComPullReqUrl :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequestReviewComment where rnf = genericRnf


-- Aeson Instances

instance FromJSON HookIssue where
  parseJSON = withObject "HookIssue" $ \o -> HookIssue
      <$> o .: "url"
      <*> o .: "labels_url"
      <*> o .: "comments_url"
      <*> o .: "events_url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "number"
      <*> o .: "title"
      <*> o .: "user"
      <*> o .: "labels"
      <*> o .: "state"
      <*> o .: "locked"
      <*> o .: "assignee"
      <*> o .: "milestone"
      <*> o .: "comments"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "closed_at"
      <*> o .: "body"

instance FromJSON HookRepository where
  parseJSON = withObject "HookRepository" $ \o -> HookRepository
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "full_name"
      <*> ((Right <$> o .: "owner") <|> (Left <$> o .: "owner")) -- try complex form first
      <*> o .: "private"
      <*> o .: "html_url"
      <*> o .:? "description" .!= T.empty
      <*> o .: "fork"
      <*> o .: "url"
      <*> o .: "forks_url"
      <*> o .: "keys_url"
      <*> o .: "collaborators_url"
      <*> o .: "teams_url"
      <*> o .: "hooks_url"
      <*> o .: "issue_events_url"
      <*> o .: "events_url"
      <*> o .: "assignees_url"
      <*> o .: "branches_url"
      <*> o .: "tags_url"
      <*> o .: "blobs_url"
      <*> o .: "git_tags_url"
      <*> o .: "git_refs_url"
      <*> o .: "trees_url"
      <*> o .: "statuses_url"
      <*> o .: "languages_url"
      <*> o .: "stargazers_url"
      <*> o .: "contributors_url"
      <*> o .: "subscribers_url"
      <*> o .: "subscription_url"
      <*> o .: "commits_url"
      <*> o .: "git_commits_url"
      <*> o .: "comments_url"
      <*> o .: "issue_comment_url"
      <*> o .: "contents_url"
      <*> o .: "compare_url"
      <*> o .: "merges_url"
      <*> o .: "archive_url"
      <*> o .: "downloads_url"
      <*> o .: "issues_url"
      <*> o .: "pulls_url"
      <*> o .: "milestones_url"
      <*> o .: "notifications_url"
      <*> o .: "labels_url"
      <*> o .: "releases_url"
      -- FIXME: Wrap optional number/stringified UTCTime in a helper function? See PushEvent fixture
      <*> ((o .: "created_at")  <|> (posixSecondsToUTCTime . fromInteger <$> o .: "created_at"))
      <*> ((o .: "updated_at")  <|> (posixSecondsToUTCTime . fromInteger <$> o .: "updated_at"))
      <*> ((o .: "pushed_at")   <|> (posixSecondsToUTCTime . fromInteger <$> o .: "pushed_at"))
      <*> o .: "git_url"
      <*> o .: "ssh_url"
      <*> o .: "clone_url"
      <*> o .: "svn_url"
      <*> o .:? "homepage"
      <*> o .: "size"
      <*> o .: "stargazers_count"
      <*> o .: "watchers_count"
      <*> o .: "language"
      <*> o .: "has_issues"
      <*> o .: "has_downloads"
      <*> o .: "has_wiki"
      <*> o .: "has_pages"
      <*> o .: "forks_count"
      <*> o .:? "mirror_url"
      <*> o .: "open_issues_count"
      <*> o .: "default_branch"

instance FromJSON HookRepositorySimple where
  parseJSON = withObject "HookRepositorySimple" $ \o -> HookRepositorySimple
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "full_name"
      <*> o .: "private"

instance FromJSON HookRepositoryLabel where
  parseJSON = withObject "HookRepositoryLabel" $ \o -> HookRepositoryLabel
      <$> o .: "url"
      <*> o .: "name"
      <*> o .: "color"

instance FromJSON HookUser where
  parseJSON = withObject "HookUser" $ \o -> HookUser
      <$> o .: "login"
      <*> o .: "id"
      <*> o .: "avatar_url"
      <*> o .: "gravatar_id"
      <*> o .: "url"
      <*> o .: "html_url"
      <*> o .: "followers_url"
      <*> o .: "following_url"
      <*> o .: "gists_url"
      <*> o .: "starred_url"
      <*> o .: "subscriptions_url"
      <*> o .: "organizations_url"
      <*> o .: "repos_url"
      <*> o .: "events_url"
      <*> o .: "received_events_url"
      <*> o .: "type"
      <*> o .: "site_admin"

instance FromJSON HookSimpleUser where
  parseJSON = withObject "HookSimpleUser" $ \o -> HookSimpleUser
      <$> o .: "name"
      <*> o .: "email"
      <*> o .:? "username"

instance FromJSON HookOrganization where
  parseJSON = withObject "HookOrganization" $ \o -> HookOrganization
      <$> o .: "login"
      <*> o .: "id"
      <*> o .: "url"
      <*> o .: "repos_url"
      <*> o .: "events_url"
      <*> o .:? "hooks_url"
      <*> o .:? "issues_url"
      <*> o .: "members_url"
      <*> o .: "public_members_url"
      <*> o .: "avatar_url"
      <*> o .:? "description" .!= T.empty

instance FromJSON HookOrganizationInvitation where
  parseJSON = withObject "HookOrganizationInvitation" $ \o -> HookOrganizationInvitation
      <$> o .: "id"
      <*> o .: "login"
      <*> o .: "email"
      <*> o .: "role"

instance FromJSON HookOrganizationMembership where
  parseJSON = withObject "HookOrganizationMembership" $ \o -> HookOrganizationMembership
      <$> o .: "url"
      <*> o .: "state"
      <*> o .: "role"
      <*> o .: "organization_url"
      <*> o .: "user"

instance FromJSON HookTeam where
  parseJSON = withObject "HookTeam" $ \o -> HookTeam
      <$> o .: "name"
      <*> o .: "id"
      <*> o .: "slug"
      <*> o .: "permission"
      <*> o .: "url"
      <*> o .: "members_url"
      <*> o .: "repositories_url"

instance FromJSON HookMilestone where
  parseJSON = withObject "HookMilestone" $ \o -> HookMilestone
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "labels_url"
      <*> o .: "id"
      <*> o .: "number"
      <*> o .: "title"
      <*> o .: "description"
      <*> o .: "creator"
      <*> o .: "open_issues"
      <*> o .: "closed_issues"
      <*> o .: "state"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "due_on"
      <*> o .: "closed_at"

instance FromJSON HookMembership where
  parseJSON = withObject "HookMembership" $ \o -> HookMembership
      <$> o .: "url"
      <*> o .: "state"
      <*> o .: "role"
      <*> o .: "organization_url"
      <*> o .: "user"

instance FromJSON HookProject where
  parseJSON = withObject "HookProject" $ \o -> HookProject
      <$> o .: "owner_url"
      <*> o .: "url"
      <*> o .: "columns_url"
      <*> o .: "id"
      <*> o .: "name"
      <*> o .: "body"
      <*> o .: "number"
      <*> o .: "state"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookProjectCard where
  parseJSON = withObject "HookProjectCard" $ \o -> HookProjectCard
      <$> o .: "url"
      <*> o .: "column_url"
      <*> o .: "column_id"
      <*> o .: "id"
      <*> o .:? "note"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "content_url"

instance FromJSON HookProjectColumn where
  parseJSON = withObject "HookProjectColumn" $ \o -> HookProjectColumn
      <$> o .: "url"
      <*> o .: "project_url"
      <*> o .: "cards_url"
      <*> o .: "id"
      <*> o .: "name"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookIssueLabels where
  parseJSON = withObject "HookIssueLabels" $ \o -> HookIssueLabels
      <$> o .:? "id"
      <*> o .: "url"
      <*> o .: "name"
      <*> o .: "color"
      <*> o .:? "default" .!= False

instance FromJSON HookCheckSuite where
  parseJSON = withObject "HookCheckSuite" $ \o -> HookCheckSuite
      <$> o .: "id"
      <*> o .:? "head_branch"
      <*> o .: "head_sha"
      <*> o .: "status"
      <*> o .:? "conclusion"
      <*> o .: "url"
      <*> o .:? "before"
      <*> o .: "after"
      <*> o .: "pull_requests"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "latest_check_runs_count"
      <*> o .:? "check_runs_url"
      <*> o .:? "head_commit"

instance FromJSON HookCheckSuiteCommit where
  parseJSON = withObject "HookCheckSuiteCommit" $ \o -> HookCheckSuiteCommit
      <$> o .: "id"
      <*> o .: "author"
      <*> o .: "committer"

instance FromJSON HookCheckRun where
  parseJSON = withObject "HookCheckRun" $ \o -> HookCheckRun
      <$> o .: "id"
      <*> o .: "head_sha"
      <*> o .: "external_id"
      <*> o .: "url"
      <*> o .: "html_url"
      <*> o .: "details_url"
      <*> o .: "status"
      <*> o .:? "conclusion"
      <*> o .: "started_at"
      <*> o .:? "completed_at"
      <*> o .: "output"
      <*> o .: "name"
      <*> o .: "check_suite"
      <*> o .: "pull_requests"

instance FromJSON HookCheckRunOutput where
  parseJSON = withObject "HookCheckRunOutput" $ \o -> HookCheckRunOutput
      <$> o .: "title"
      <*> o .: "summary"
      <*> o .: "text"
      <*> o .: "annotations_count"
      <*> o .: "annotations_url"

instance FromJSON HookCheckRunRequestedAction where
  parseJSON = withObject "HookCheckRunRequestedAction" $ \o -> HookCheckRunRequestedAction
      <$> o .: "identifier"

instance FromJSON HookChecksInstallation where
  parseJSON = withObject "HookChecksInstallation" $ \o -> HookChecksInstallation
      <$> o .: "id"

instance FromJSON HookChecksPullRequest where
  parseJSON = withObject "HookChecksPullRequest" $ \o -> HookChecksPullRequest
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "number"
      <*> o .: "head"
      <*> o .: "base"

instance FromJSON HookChecksPullRequestTarget where
    parseJSON = withObject "PullRequestTarget" $ \o -> HookChecksPullRequestTarget
      <$> o .: "sha"
      <*> o .: "ref"
      <*> o .: "repo"

instance FromJSON HookChecksPullRequestRepository where
  parseJSON = withObject "HookChecksPullRequestRepository" $ \o -> HookChecksPullRequestRepository
      <$> o .: "id"
      <*> o .: "url"
      <*> o .: "name"

instance FromJSON HookCommit where
  parseJSON = withObject "HookCommit" $ \o -> HookCommit
      <$> (o .: "sha" <|> o .: "id")
      <*> o .: "url"
      <*> o .:? "html_url"
      <*> o .:? "comments_url"
      <*> ((Right <$> o .: "author")      <|> (Left <$> o .: "author"))       -- try complex form first
      <*> ((Right <$> o .: "committer")   <|> (Left <$> o .: "committer"))    -- try complex form first

instance FromJSON HookRelease where
  parseJSON = withObject "HookRelease" $ \o -> HookRelease
      <$> o .: "url"
      <*> o .: "assets_url"
      <*> o .: "upload_url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "tag_name"
      <*> o .: "target_commitish"
      <*> o .:? "name"
      <*> o .: "draft"
      <*> o .: "author"
      <*> o .: "prerelease"
      <*> o .: "created_at"
      <*> o .:? "published_at"
      <*> o .: "tarball_url"
      <*> o .: "zipball_url"
      <*> o .:? "body"

instance FromJSON HookPullRequest where
  parseJSON = withObject "HookPullRequest" $ \o -> HookPullRequest
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "html_url"
      <*> o .: "diff_url"
      <*> o .: "patch_url"
      <*> o .: "issue_url"
      <*> o .: "number"
      <*> o .: "state"
      <*> o .: "locked"
      <*> o .: "title"
      <*> o .: "user"
      <*> o .:? "body" .!= ""
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "closed_at"
      <*> o .:? "merged_at"
      <*> o .:? "merge_commit_sha"
      <*> o .:? "assignee"
      <*> o .:? "milestone"
      <*> o .: "commits_url"
      <*> o .: "review_comments_url"
      <*> o .: "review_comment_url"
      <*> o .: "comments_url"
      <*> o .: "statuses_url"
      <*> o .: "base"
      <*> o .: "head"
      -- <*> o .: "merged"
      -- <*> o .: "mergeable"
      -- <*> o .: "mergeable_state"
      -- <*> o .: "merged_by"
      <*> o .:? "comments"
      <*> o .:? "review_comments"
      <*> o .:? "commits"
      <*> o .:? "additions"
      <*> o .:? "deletions"
      <*> o .:? "changed_files"

instance FromJSON PullRequestTarget where
    parseJSON = withObject "PullRequestTarget" $ \o -> PullRequestTarget
      <$> o .: "sha"
      <*> o .: "user"
      <*> o .: "repo"
      <*> o .: "label"
      <*> o .: "ref"

instance FromJSON HookPullRequestReview where
  parseJSON = withObject "HookPullRequestReview" $ \o -> HookPullRequestReview
      <$> o .: "id"
      <*> o .: "user"
      <*> o .: "body"
      <*> o .: "submitted_at"
      <*> o .: "state"
      <*> o .: "html_url"
      <*> o .: "pull_request_url"

instance FromJSON HookInstallation where
  parseJSON = withObject "HookInstallation" $ \o -> HookInstallation
      <$> o .: "id"
      <*> o .: "account"
      <*> o .: "repository_selection"
      <*> o .: "access_tokens_url"
      <*> o .: "repositories_url"

instance FromJSON HookDeployment where
  parseJSON = withObject "HookDeployment" $ \o -> HookDeployment
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "sha"
      <*> o .: "ref"
      <*> o .: "task"
      <*> o .: "environment"
      <*> o .:? "description"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "statuses_url"
      <*> o .: "repository_url"

instance FromJSON HookDeploymentStatus where
  parseJSON = withObject "HookDeploymentStatus" $ \o -> HookDeploymentStatus
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "state"
      <*> o .: "creator"
      <*> o .:? "description"
      <*> o .:? "target_url"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "deployment_url"
      <*> o .: "repository_url"

instance FromJSON HookWikiPage where
  parseJSON = withObject "HookWikiPage" $ \o -> HookWikiPage
      <$> o .: "page_name"
      <*> o .: "title"
      <*> o .:? "summary"
      <*> o .: "action"
      <*> o .: "sha"
      <*> o .: "html_url"

instance FromJSON HookPageBuildResult where
  parseJSON = withObject "HookPageBuildResult" $ \o -> HookPageBuildResult
      <$> o .: "url"
      <*> o .: "status"
      <*> (o .: "error" >>= \e -> e .:? "message")
      <*> o .: "pusher"
      <*> o .: "commit"
      <*> o .: "duration"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookIssueComment where
  parseJSON = withObject "HookIssueComment" $ \o -> HookIssueComment
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "issue_url"
      <*> o .: "id"
      <*> o .: "user"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "body"

instance FromJSON HookCommitComment where
  parseJSON = withObject "HookCommitComment" $ \o -> HookCommitComment
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "user"
      <*> o .:? "position"
      <*> o .:? "line"
      <*> o .:? "path"
      <*> o .: "commit_id"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "body"

instance FromJSON HookPullRequestReviewComment where
  parseJSON = withObject "HookPullRequestReviewComment" $ \o -> HookPullRequestReviewComment
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "diff_hunk"
      <*> o .: "path"
      <*> o .: "position"
      <*> o .: "original_position"
      <*> o .: "commit_id"
      <*> o .: "original_commit_id"
      <*> o .: "user"
      <*> o .: "body"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "html_url"
      <*> o .: "pull_request_url"
