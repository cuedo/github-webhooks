{-# LANGUAGE Trustworthy #-}
{-|
Module      : Github.Data.Webhooks.Events
Copyright   : (c) ONROCK, 2018
License     : MIT
Maintainer  : Kyle Van Berendonck <foss@onrock.online>

This module contains types that represent GitHub webhook's events.
-}
module GitHub.Data.Webhooks.Events
    ( EventHasSender(..)
    , EventHasRepo(..)
      --
    , CheckSuiteEventAction(..)
    , CheckSuiteEvent(..)
      --
    , CheckRunEventAction(..)
    , CheckRunEvent(..)
      --
    , CommitCommentEvent(..)
    , CommitCommentEventAction(..)
      --
    , CreateEvent(..)
      --
    , DeleteEvent(..)
      --
    , DeploymentEvent(..)
      --
    , DeploymentStatusEvent(..)
      -- Deprecated events
    , DownloadEvent(..)
    , FollowEvent(..)
    , ForkEvent(..)
    , ForkApplyEvent(..)
    , GistEvent(..)
      --
    , GollumEvent(..)
      --
    , InstallationEvent(..)
    , InstallationEventAction(..)
      --
    , InstallationRepositoriesEvent(..)
    , InstallationRepoEventAction(..)
      --
    , IssueCommentEvent(..)
    , IssueCommentEventAction(..)
      --
    , IssuesEvent(..)
    , IssuesEventAction(..)
      --
    , LabelEvent(..)
    , LabelEventAction(..)
      --
    , MemberEvent(..)
    , MemberEventAction(..)
      --
    , MembershipEvent(..)
    , MembershipEventAction(..)
      --
    , MilestoneEvent(..)
    , MilestoneEventAction(..)
      --
    , OrganizationEvent(..)
    , OrganizationEventAction(..)
      --
    , OrgBlockEvent(..)
    , OrgBlockEventAction(..)
      --
    , PageBuildEvent(..)
      --
    , ProjectCardEvent(..)
    , ProjectCardEventAction(..)
      --
    , ProjectColumnEvent(..)
    , ProjectColumnEventAction(..)
      --
    , ProjectEvent(..)
    , ProjectEventAction(..)
      --
    , PublicEvent(..)
      --
    , PullRequestEvent(..)
    , PullRequestEventAction(..)
      --
    , PullRequestReviewEvent(..)
    , PullRequestReviewEventAction(..)
      --
    , PullRequestReviewCommentEvent(..)
    , PullRequestReviewCommentEventAction(..)
      --
    , PushEvent(..)
      --
    , ReleaseEvent(..)
    , ReleaseEventAction(..)
      --
    , RepositoryEvent(..)
    , RepositoryEventAction(..)
      --
    , StatusEvent(..)
    , StatusEventState(..)
      --
    , TeamEvent(..)
    , TeamEventAction(..)
      --
    , TeamAddEvent(..)
      --
    , WatchEvent(..)
    , WatchEventAction(..)
    ) where

import           Data.Aeson               (FromJSON(..), withObject, withText, (.:), (.:?), (.!=))
import           Control.Applicative      ((<*>), pure)
import           Control.DeepSeq          (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Data                (Data, Typeable)
import           Data.Functor             ((<$>))
import           Data.Monoid              (mempty)
import           Data.Time                (UTCTime)
import           Data.Text                (Text)
import           Data.Vector              (Vector)
import           GHC.Generics             (Generic)

import           GitHub.Data.Webhooks.Payload

-- | Represents an event that contains its sender information.
class EventHasSender eventKind where
    -- | Provides the sender context of a Webhook event.
    senderOfEvent :: eventKind -> HookUser

-- | Represents an event that contains its repository information.
class EventHasRepo eventKind where
    -- | Provides the repository context of a Webhook event.
    repoForEvent :: eventKind -> HookRepository

-- | Represents the "action" field in the
-- 'CheckSuiteEvent' payload.
data CheckSuiteEventAction
    -- | Decodes from "completed"
    = CheckSuiteEventActionCompleted
    -- | Decodes from "requested"
    | CheckSuiteEventActionRequested
    -- | Decodes from "rerequested"
    | CheckSuiteEventActionRerequested
    -- | The result of decoding an unknown check suite event action type
    | CheckSuiteEventActionOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData CheckSuiteEventAction where rnf = genericRnf

instance FromJSON CheckSuiteEventAction where
  parseJSON = withText "Check suite event action" $ \t ->
      case t of
          "completed"          -> pure CheckSuiteEventActionCompleted
          "requested"          -> pure CheckSuiteEventActionRequested
          "rerequested"        -> pure CheckSuiteEventActionRerequested
          _                    -> pure (CheckSuiteEventActionOther t)

-- | Triggered when a check suite is completed, requested, or rerequested.
-- See <https://developer.github.com/v3/activity/events/types/#checksuiteevent>.
data CheckSuiteEvent = CheckSuiteEvent
    { evCheckSuiteAction              :: !CheckSuiteEventAction
    , evCheckSuiteCheckSuite          :: !HookCheckSuite
    , evCheckSuiteRepository          :: !HookRepository
    , evCheckSuiteOrganization        :: !(Maybe HookOrganization)
    , evCheckSuiteSender              :: !HookUser
    , evCheckSuiteInstallation        :: !(Maybe HookChecksInstallation)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CheckSuiteEvent where senderOfEvent = evCheckSuiteSender
instance EventHasRepo CheckSuiteEvent where repoForEvent = evCheckSuiteRepository
instance NFData CheckSuiteEvent where rnf = genericRnf

-- | Represents the "action" field in the
-- 'CheckRunEvent' payload.
data CheckRunEventAction
    -- | Decodes from "created"
    = CheckRunEventActionCreated
    -- | Decodes from "completed"
    | CheckRunEventActionCompleted
    -- | Decodes from "rerequested"
    | CheckRunEventActionRerequested
    -- | Decodes from "requested_action"
    | CheckRunEventActionRequestedAction
    -- | The result of decoding an unknown check run event action type
    | CheckRunEventActionOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData CheckRunEventAction where rnf = genericRnf

instance FromJSON CheckRunEventAction where
  parseJSON = withText "Check suite event action" $ \t ->
      case t of
          "created"            -> pure CheckRunEventActionCreated
          "completed"          -> pure CheckRunEventActionCompleted
          "requested"          -> pure CheckRunEventActionRerequested
          "requested_action"   -> pure CheckRunEventActionRequestedAction
          _                    -> pure (CheckRunEventActionOther t)

-- | Triggered when a check run is created, rerequested, completed, or has a requested_action.
-- See <https://developer.github.com/v3/activity/events/types/#checkrunevent>.
data CheckRunEvent = CheckRunEvent
    { evCheckRunAction              :: !CheckRunEventAction
    , evCheckRunCheckRun            :: !HookCheckRun
    , evCheckRunRequestedAction     :: !(Maybe HookCheckRunRequestedAction)
    , evCheckRunRepository          :: !HookRepository
    , evCheckRunOrganization        :: !(Maybe HookOrganization)
    , evCheckRunSender              :: !HookUser
    , evCheckRunInstallation        :: !(Maybe HookChecksInstallation)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CheckRunEvent where senderOfEvent = evCheckRunSender
instance EventHasRepo CheckRunEvent where repoForEvent = evCheckRunRepository
instance NFData CheckRunEvent where rnf = genericRnf

-- | Represents the "action" field in the
-- 'CommitCommentEvent' payload.
data CommitCommentEventAction
    -- | Decodes from "created"
    = CommitCommentActionCreated
    -- | The result of decoding an unknown commit comment event action type
    | CommitCommentActionOther !Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData CommitCommentEventAction where rnf = genericRnf

instance FromJSON CommitCommentEventAction where
  parseJSON = withText "Commit comment event action" $ \t ->
      case t of
          "created"       -> pure CommitCommentActionCreated
          _               -> pure (CommitCommentActionOther t)

-- | Triggered when a commit comment is created.
-- See <https://developer.github.com/v3/activity/events/types/#commitcommentevent>.
data CommitCommentEvent = CommitCommentEvent
    { evCommitCommentAction     :: !CommitCommentEventAction
    , evCommitCommentPayload    :: !HookCommitComment
    , evCommitCommentRepo       :: !HookRepository
    , evCommitCommentSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CommitCommentEvent where senderOfEvent = evCommitCommentSender
instance EventHasRepo CommitCommentEvent where repoForEvent = evCommitCommentRepo
instance NFData CommitCommentEvent where rnf = genericRnf


-- | Represents a created repository, branch, or tag.
-- Note: webhooks will not receive this event for created repositories.
-- Additionally, webhooks will not receive this event for tags if more than three tags are pushed at once.
-- See <https://developer.github.com/v3/activity/events/types/#createevent>.
data CreateEvent = CreateEvent
    { evCreateRef               :: !Text
    , evCreateRefType           :: !Text
    , evCreateMasterBranch      :: !Text
    , evCreateDescription       :: !Text
    , evCreatePusherType        :: !OwnerType
    , evCreateRepo              :: !HookRepository
    , evCreateSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender CreateEvent where senderOfEvent = evCreateSender
instance EventHasRepo CreateEvent where repoForEvent = evCreateRepo
instance NFData CreateEvent where rnf = genericRnf


-- | Represents a deleted branch or tag.
-- Note: webhooks will not receive this event for tags if more than three tags are deleted at once.
-- See <https://developer.github.com/v3/activity/events/types/#deleteevent>.
data DeleteEvent = DeleteEvent
    { evDeleteRef               :: !Text
    , evDeleteRefType           :: !Text
    , evDeletePusherType        :: !OwnerType
    , evDeleteRepo              :: !HookRepository
    , evDeleteSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender DeleteEvent where senderOfEvent = evDeleteSender
instance EventHasRepo DeleteEvent where repoForEvent = evDeleteRepo
instance NFData DeleteEvent where rnf = genericRnf


-- | Represents a deployment.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#deploymentevent>.
data DeploymentEvent = DeploymentEvent
    { evDeploymentInfo          :: !HookDeployment
    , evDeploymentRepo          :: !HookRepository
    , evDeploymentSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender DeploymentEvent where senderOfEvent = evDeploymentSender
instance EventHasRepo DeploymentEvent where repoForEvent = evDeploymentRepo
instance NFData DeploymentEvent where rnf = genericRnf


-- | Represents a deployment status.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#deploymentstatusevent>.
data DeploymentStatusEvent = DeploymentStatusEvent
    { evDeplStatusInfo          :: !HookDeploymentStatus
    , evDeplStatusDeployment    :: !HookDeployment
    , evDeplStatusRepo          :: !HookRepository
    , evDeplStatusSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender DeploymentStatusEvent where senderOfEvent = evDeplStatusSender
instance EventHasRepo DeploymentStatusEvent where repoForEvent = evDeplStatusRepo
instance NFData DeploymentStatusEvent where rnf = genericRnf


-- | Triggered when a new download is created.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#downloadevent>.
data DownloadEvent = DownloadEvent

-- | Triggered when a user follows another user.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#downloadevent>.
data FollowEvent = FollowEvent

-- | Triggered when a user forks a repository.
-- See <https://developer.github.com/v3/activity/events/types/#forkevent>.
data ForkEvent = ForkEvent
    { evForkDestination         :: !HookRepository
    , evForkSource              :: !HookRepository
    , evForkSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender ForkEvent where senderOfEvent = evForkSender
instance EventHasRepo ForkEvent where repoForEvent = evForkSource
instance NFData ForkEvent where rnf = genericRnf


-- | Triggered when a patch is applied in the Fork Queue.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#forkapplyevent>.
data ForkApplyEvent = ForkApplyEvent


-- | Triggered when a Gist is created or updated.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#gistevent>.
data GistEvent = GistEvent


-- | Triggered when a Wiki page is created or updated.
-- See <https://developer.github.com/v3/activity/events/types/#gollumevent>.
data GollumEvent = GollumEvent
    { evGollumPages             :: !(Vector HookWikiPage)
    , evGollumRepo              :: !HookRepository
    , evGollumSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender GollumEvent where senderOfEvent = evGollumSender
instance EventHasRepo GollumEvent where repoForEvent = evGollumRepo
instance NFData GollumEvent where rnf = genericRnf


data InstallationEventAction
  -- | Decodes from "created"
  = InstallationCreatedAction
  -- | Decodes from "deleted"
  | InstallationDeletedAction
  -- | The result of decoding an unknown installation event action type
  | InstallationActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData InstallationEventAction where rnf = genericRnf

instance FromJSON InstallationEventAction where
  parseJSON = withText "Installation event action" $ \t ->
    case t of
        "created"       -> pure InstallationCreatedAction
        "deleted"       -> pure InstallationDeletedAction
        _               -> pure (InstallationActionOther t)

-- | Triggered when a GitHub App has been installed or uninstalled.
-- See <https://developer.github.com/v3/activity/events/types/#installationevent>.
data InstallationEvent = InstallationEvent
    { evInstallationAction      :: !InstallationEventAction
    , evInstallationInfo        :: !HookInstallation
    , evInstallationRepos       :: !(Vector HookRepositorySimple)
    , evInstallationSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender InstallationEvent where senderOfEvent = evInstallationSender
instance NFData InstallationEvent where rnf = genericRnf


data InstallationRepoEventAction
  -- | Decodes from "created"
  = InstallationRepoCreatedAction
  -- | Decodes from "removed"
  | InstallationRepoRemovedAction
  -- | The result of decoding an unknown installation repo event action type
  | InstallationRepoActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData InstallationRepoEventAction where rnf = genericRnf

instance FromJSON InstallationRepoEventAction where
  parseJSON = withText "Installation repo event action" $ \t ->
    case t of
        "created"       -> pure InstallationRepoCreatedAction
        "removed"       -> pure InstallationRepoRemovedAction
        _               -> pure (InstallationRepoActionOther t)

-- | Triggered when a repository is added or removed from an installation.
-- See <https://developer.github.com/v3/activity/events/types/#installationrepositoriesevent>.
data InstallationRepositoriesEvent = InstallationRepositoriesEvent
    { evInstallationRepoAction  :: !InstallationRepoEventAction
    , evInstallationRepoInfo    :: !HookInstallation
    , evInstallationRepoSel     :: !Text
    , evInstallationReposAdd    :: !(Vector HookRepositorySimple)
    , evInstallationReposRemove :: !(Vector HookRepositorySimple)
    , evInstallationReposSender :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender InstallationRepositoriesEvent where senderOfEvent = evInstallationReposSender
instance NFData InstallationRepositoriesEvent where rnf = genericRnf


data IssueCommentEventAction
  -- | Decodes from "created"
  = IssueCommentCreatedAction
  -- | Decodes from "edited"
  | IssueCommentEditedAction
  -- | Decodes from "deleted"
  | IssueCommentDeletedAction
  -- | The result of decoding an unknown issue comment event action type
  | IssueCommentActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData IssueCommentEventAction where rnf = genericRnf

instance FromJSON IssueCommentEventAction where
  parseJSON = withText "Issue comment event action" $ \t ->
    case t of
        "created"       -> pure IssueCommentCreatedAction
        "edited"        -> pure IssueCommentEditedAction
        "deleted"       -> pure IssueCommentDeletedAction
        _               -> pure (IssueCommentActionOther t)

-- | Triggered when an issue comment is created, edited, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#issuecommentevent>.
data IssueCommentEvent = IssueCommentEvent
    { evIssueCommentAction      :: !IssueCommentEventAction
    , evIssueCommentIssue       :: !HookIssue
    , evIssueCommentPayload     :: !HookIssueComment
    , evIssueCommentRepo        :: !HookRepository
    , evIssueCommentSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender IssueCommentEvent where senderOfEvent = evIssueCommentSender
instance EventHasRepo IssueCommentEvent where repoForEvent = evIssueCommentRepo
instance NFData IssueCommentEvent where rnf = genericRnf


data IssuesEventAction
  -- | Decodes from "assigned"
  = IssuesAssignedAction
  -- | Decodes from "unassigned"
  | IssuesUnassignedAction
  -- | Decodes from "labeled"
  | IssuesLabeledAction
  -- | Decodes from "unlabeled"
  | IssuesUnlabeledAction
  -- | Decodes from "opened"
  | IssuesOpenedAction
  -- | Decodes from "edited"
  | IssuesEditedAction
  -- | Decodes from "milestoned"
  | IssuesMilestonedAction
  -- | Decodes from "demilestoned"
  | IssuesDemilestonedAction
  -- | Decodes from "closed"
  | IssuesClosedAction
  -- | Decodes from "reopened"
  | IssuesReopenedAction
  -- | The result of decoding an unknown issue comment event action type
  | IssuesActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData IssuesEventAction where rnf = genericRnf

instance FromJSON IssuesEventAction where
  parseJSON = withText "Issue comment event action" $ \t ->
    case t of
        "assigned"      -> pure IssuesAssignedAction
        "unassigned"    -> pure IssuesUnassignedAction
        "labeled"       -> pure IssuesLabeledAction
        "unlabeled"     -> pure IssuesUnlabeledAction
        "opened"        -> pure IssuesOpenedAction
        "edited"        -> pure IssuesEditedAction
        "milestoned"    -> pure IssuesMilestonedAction
        "demilestoned"  -> pure IssuesDemilestonedAction
        "closed"        -> pure IssuesClosedAction
        "reopened"      -> pure IssuesReopenedAction
        _               -> pure (IssuesActionOther t)

-- | Triggered when an issue is assigned, unassigned, labeled,
--  unlabeled, opened, edited, milestoned, demilestoned, closed, or reopened.
-- See <https://developer.github.com/v3/activity/events/types/#issuesevent>.
data IssuesEvent = IssuesEvent
    { evIssuesEventAction       :: !IssuesEventAction
    , evIssuesEventIssue        :: !HookIssue
    , evIssuesEventRepo         :: !HookRepository
    , evIssuesEventSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender IssuesEvent where senderOfEvent = evIssuesEventSender
instance EventHasRepo IssuesEvent where repoForEvent = evIssuesEventRepo
instance NFData IssuesEvent where rnf = genericRnf


data LabelEventAction
  -- | Decodes from "created"
  = LabelCreatedAction
  -- | Decodes from "edited"
  | LabelEditedAction
  -- | Decodes from "deleted"
  | LabelDeletedAction
  -- | The result of decoding an unknown label event action type
  | LabelActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData LabelEventAction where rnf = genericRnf

instance FromJSON LabelEventAction where
  parseJSON = withText "Label event action" $ \t ->
    case t of
        "created"       -> pure LabelCreatedAction
        "edited"        -> pure LabelEditedAction
        "deleted"       -> pure LabelDeletedAction
        _               -> pure (LabelActionOther t)

-- | Triggered when a repository's label is created, edited, or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#labelevent>.
data LabelEvent = LabelEvent
    { evLabelEventAction        :: !LabelEventAction
    , evLabelEventPayload       :: !HookRepositoryLabel
    , evLabelEventRepo          :: !HookRepository
    , evLabelEventOrganization  :: !(Maybe HookOrganization)
    , evLabelEventSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender LabelEvent where senderOfEvent = evLabelEventSender
instance EventHasRepo LabelEvent where repoForEvent = evLabelEventRepo
instance NFData LabelEvent where rnf = genericRnf


data MemberEventAction
  -- | Decodes from "added"
  = MemberAddedAction
  -- | Decodes from "edited"
  | MemberEditedAction
  -- | Decodes from "deleted"
  | MemberDeletedAction
  -- | The result of decoding an unknown label event action type
  | MemberActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData MemberEventAction where rnf = genericRnf

instance FromJSON MemberEventAction where
  parseJSON = withText "Member event action" $ \t ->
    case t of
        "added"         -> pure MemberAddedAction
        "edited"        -> pure MemberEditedAction
        "deleted"       -> pure MemberDeletedAction
        _               -> pure (MemberActionOther t)

-- | Triggered when a user is added or removed as a collaborator to a repository, or has their permissions changed.
-- See <https://developer.github.com/v3/activity/events/types/#memberevent>.
data MemberEvent = MemberEvent
    { evMemberAction            :: !MemberEventAction
    , evMemberUser              :: !HookUser
    , evMemberRepo              :: !HookRepository
    , evMemberSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender MemberEvent where senderOfEvent = evMemberSender
instance EventHasRepo MemberEvent where repoForEvent = evMemberRepo
instance NFData MemberEvent where rnf = genericRnf


data MembershipEventAction
  -- | Decodes from "added"
  = MembershipAddedAction
  -- | Decodes from "removed"
  | MembershipRemovedAction
  -- | The result of decoding an unknown label event action type
  | MembershipActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData MembershipEventAction where rnf = genericRnf

instance FromJSON MembershipEventAction where
  parseJSON = withText "Membership event action" $ \t ->
    case t of
        "added"         -> pure MembershipAddedAction
        "removed"       -> pure MembershipRemovedAction
        _               -> pure (MembershipActionOther t)

-- | Triggered when a user is added or removed from a team.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#membershipevent>.
data MembershipEvent = MembershipEvent
    { evMembershipAction        :: !MembershipEventAction
    , evMembershipScope         :: !Text        -- ^ Current can only be "team"
    , evMembershipUser          :: !HookUser
    , evMembershipTeam          :: !HookTeam
    , evMembershipOrg           :: !HookOrganization
    , evMembershipSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender MembershipEvent where senderOfEvent = evMembershipSender
instance NFData MembershipEvent where rnf = genericRnf


data MilestoneEventAction
  -- | Decodes from "created"
  = MilestoneCreatedAction
  -- | Decodes from "closed"
  | MilestoneClosedAction
  -- | Decodes from "opened"
  | MilestoneOpenedAction
  -- | Decodes from "edited"
  | MilestoneEditedAction
  -- | Decodes from "deleted"
  | MilestoneDeletedAction
  -- | The result of decoding an unknown label event action type
  | MilestoneActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData MilestoneEventAction where rnf = genericRnf

instance FromJSON MilestoneEventAction where
  parseJSON = withText "Milestone event action" $ \t ->
    case t of
        "created"       -> pure MilestoneCreatedAction
        "closed"        -> pure MilestoneClosedAction
        "opened"        -> pure MilestoneOpenedAction
        "edited"        -> pure MilestoneEditedAction
        "deleted"       -> pure MilestoneDeletedAction
        _               -> pure (MilestoneActionOther t)

-- | Triggered when a milestone is created, closed, opened, edited, or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#milestoneevent>.
data MilestoneEvent = MilestoneEvent
    { evMilestoneAction         :: !MilestoneEventAction
    , evMilestoenPayload        :: !HookMilestone
    , evMilestoneRepo           :: !HookRepository
    , evMilestoneOrg            :: !HookOrganization
    , evMilestoneSender         :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender MilestoneEvent where senderOfEvent = evMilestoneSender
instance EventHasRepo MilestoneEvent where repoForEvent = evMilestoneRepo
instance NFData MilestoneEvent where rnf = genericRnf


data OrganizationEventAction
  -- | Decodes from "member_added"
  = OrgMemberAddedAction
  -- | Decodes from "member_removed"
  | OrgMemberRemovedAction
  -- | Decodes from "member_invited"
  | OrgMemberInvitedAction
  -- | The result of decoding an unknown label event action type
  | OrgActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData OrganizationEventAction where rnf = genericRnf

instance FromJSON OrganizationEventAction where
  parseJSON = withText "Organization event action" $ \t ->
    case t of
        "member_added"   -> pure OrgMemberAddedAction
        "member_removed" -> pure OrgMemberRemovedAction
        "member_invited" -> pure OrgMemberInvitedAction
        _                -> pure (OrgActionOther t)

-- | Triggered when a user is added, removed, or invited to an Organization.
-- Events of this type are not visible in timelines. These events are only used to trigger organization hooks.
-- See <https://developer.github.com/v3/activity/events/types/#organizationevent>.
data OrganizationEvent = OrganizationEvent
    { evOrganizationAction      :: !OrganizationEventAction
    , evOrganizationInvitation  :: !HookOrganizationInvitation
    , evOrganizationMembership  :: !HookOrganizationMembership
    , evOrganizationOrg         :: !HookOrganization
    , evOrganizationSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender OrganizationEvent where senderOfEvent = evOrganizationSender
instance NFData OrganizationEvent where rnf = genericRnf


data OrgBlockEventAction
  -- | Decodes from "blocked"
  = OrgBlockBlockedAction
  -- | Decodes from "unblocked"
  | OrgBlockUnblockedAction
  -- | The result of decoding an unknown org block event action type
  | OrgBlockActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData OrgBlockEventAction where rnf = genericRnf

instance FromJSON OrgBlockEventAction where
  parseJSON = withText "Organization event action" $ \t ->
    case t of
        "blocked"       -> pure OrgBlockBlockedAction
        "unblocked"     -> pure OrgBlockUnblockedAction
        _               -> pure (OrgBlockActionOther t)

-- | Triggered when an organization blocks or unblocks a user.
-- See <https://developer.github.com/v3/activity/events/types/#orgblockevent>.
data OrgBlockEvent = OrgBlockEvent
    { evOrgBlockAction          :: !OrgBlockEventAction
    , evOrgBlockUser            :: !HookUser
    , evOrgBlockOrg             :: !HookOrganization
    , evOrgBlockSender          :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender OrgBlockEvent where senderOfEvent = evOrgBlockSender
instance NFData OrgBlockEvent where rnf = genericRnf


-- | Represents an attempted build of a GitHub Pages site, whether successful or not.
-- Triggered on push to a GitHub Pages enabled branch (gh-pages for project pages, master for user and organization pages).
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#pagebuildevent>.
data PageBuildEvent = PageBuildEvent
    { evPageBuildId             :: !Int
    , evPageBuildResult         :: !HookPageBuildResult
    , evPageBuildRepo           :: !HookRepository
    , evPageBuildSender         :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PageBuildEvent where senderOfEvent = evPageBuildSender
instance EventHasRepo PageBuildEvent where repoForEvent = evPageBuildRepo
instance NFData PageBuildEvent where rnf = genericRnf


data ProjectCardEventAction
  -- | Decodes from "created"
  = ProjectCardCreatedAction
  -- | Decodes from "edited"
  | ProjectCardEditedAction
  -- | Decodes from "converted"
  | ProjectCardConvertedAction
  -- | Decodes from "moved"
  | ProjectCardMovedAction
  -- | Decodes from "deleted"
  | ProjectCardDeletedAction
  -- | The result of decoding an unknown project card event action type
  | ProjectCardActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData ProjectCardEventAction where rnf = genericRnf

instance FromJSON ProjectCardEventAction where
  parseJSON = withText "Project card event action" $ \t ->
    case t of
        "created"       -> pure ProjectCardCreatedAction
        "edited"        -> pure ProjectCardEditedAction
        "converted"     -> pure ProjectCardConvertedAction
        "moved"         -> pure ProjectCardMovedAction
        "deleted"       -> pure ProjectCardDeletedAction
        _               -> pure (ProjectCardActionOther t)

-- | Triggered when a project card is created, updated, moved, converted to an issue, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectcardevent>.
data ProjectCardEvent = ProjectCardEvent
    { evProjectCardAction       :: !ProjectCardEventAction
    , evProjectCardPayload      :: !HookProjectCard
    , evProjectCardRepo         :: !HookRepository
    , evProjectCardOrg          :: !HookOrganization
    , evProjectCardSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender ProjectCardEvent where senderOfEvent = evProjectCardSender
instance EventHasRepo ProjectCardEvent where repoForEvent = evProjectCardRepo
instance NFData ProjectCardEvent where rnf = genericRnf


data ProjectColumnEventAction
  -- | Decodes from "created"
  = ProjectColumnCreatedAction
  -- | Decodes from "edited"
  | ProjectColumnEditedAction
  -- | Decodes from "moved"
  | ProjectColumnMovedAction
  -- | Decodes from "deleted"
  | ProjectColumnDeletedAction
  -- | The result of decoding an unknown project card event action type
  | ProjectColumnActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData ProjectColumnEventAction where rnf = genericRnf

instance FromJSON ProjectColumnEventAction where
  parseJSON = withText "Project column event action" $ \t ->
    case t of
        "created"       -> pure ProjectColumnCreatedAction
        "edited"        -> pure ProjectColumnEditedAction
        "moved"         -> pure ProjectColumnMovedAction
        "deleted"       -> pure ProjectColumnDeletedAction
        _               -> pure (ProjectColumnActionOther t)

-- | Triggered when a project column is created, updated, moved, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectcolumnevent>.
data ProjectColumnEvent = ProjectColumnEvent
    { evProjectColumnAction     :: !ProjectColumnEventAction
    , evProjectColumnPayload    :: !HookProjectColumn
    , evProjectColumnRepo       :: !HookRepository
    , evProjectColumnOrg        :: !HookOrganization
    , evProjectColumnSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender ProjectColumnEvent where senderOfEvent = evProjectColumnSender
instance EventHasRepo ProjectColumnEvent where repoForEvent = evProjectColumnRepo
instance NFData ProjectColumnEvent where rnf = genericRnf


data ProjectEventAction
  -- | Decodes from "created"
  = ProjectCreatedAction
  -- | Decodes from "edited"
  | ProjectEditedAction
  -- | Decodes from "closed"
  | ProjectClosedAction
  -- | Decodes from "reopened"
  | ProjectReopenedAction
  -- | Decodes from "deleted"
  | ProjectDeletedAction
  -- | The result of decoding an unknown project event action type
  | ProjectActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData ProjectEventAction where rnf = genericRnf

instance FromJSON ProjectEventAction where
  parseJSON = withText "Project event action" $ \t ->
    case t of
        "created"       -> pure ProjectCreatedAction
        "edited"        -> pure ProjectEditedAction
        "closed"        -> pure ProjectClosedAction
        "reopened"      -> pure ProjectReopenedAction
        "deleted"       -> pure ProjectDeletedAction
        _               -> pure (ProjectActionOther t)

-- | Triggered when a project is created, updated, closed, reopened, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectevent>.
data ProjectEvent = ProjectEvent
    { evProjectEventAction      :: !ProjectEventAction
    , evProjectPayload          :: !HookProject
    , evProjectRepo             :: !HookRepository
    , evProjectOrganization     :: !HookOrganization
    , evProjectSender           :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender ProjectEvent where senderOfEvent = evProjectSender
instance EventHasRepo ProjectEvent where repoForEvent = evProjectRepo
instance NFData ProjectEvent where rnf = genericRnf


-- | Triggered when a private repository is open sourced. Without a doubt: the best GitHub event.
-- See <https://developer.github.com/v3/activity/events/types/#publicevent>.
data PublicEvent = PublicEvent
    { evPublicEventRepo         :: !HookRepository
    , evPublicEventSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PublicEvent where senderOfEvent = evPublicEventSender
instance EventHasRepo PublicEvent where repoForEvent = evPublicEventRepo
instance NFData PublicEvent where rnf = genericRnf


data PullRequestEventAction
  -- | Decodes from "assigned"
  = PullRequestAssignedAction
  -- | Decodes from "unassigned"
  | PullRequestUnassignedAction
  -- | Decodes from "review_requsted"
  | PullRequestReviewRequestedAction
  -- | Decodes from "review_request_removed"
  | PullRequestReviewRequestRemovedAction
  -- | Decodes from "labeled"
  | PullRequestLabeledAction
  -- | Decodes from "unlabeled"
  | PullRequestUnlabeledAction
  -- | Decodes from "opened"
  | PullRequestOpenedAction
  -- | Decodes from "edited"
  | PullRequestEditedAction
  -- | Decodes from "closed"
  | PullRequestClosedAction
  -- | Decodes from "reopened"
  | PullRequestReopenedAction
  -- | The result of decoding an unknown pull request event action type
  | PullRequestActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestEventAction where rnf = genericRnf

instance FromJSON PullRequestEventAction where
  parseJSON = withText "Pull request event action" $ \t ->
    case t of
        "assigned"      -> pure PullRequestAssignedAction
        "unassigned"    -> pure PullRequestUnassignedAction
        "review_requsted" -> pure PullRequestReviewRequestedAction
        "review_request_removed" -> pure PullRequestReviewRequestRemovedAction
        "labeled"       -> pure PullRequestLabeledAction
        "unlabeled"     -> pure PullRequestUnlabeledAction
        "opened"        -> pure PullRequestOpenedAction
        "edited"        -> pure PullRequestEditedAction
        "closed"        -> pure PullRequestClosedAction
        "reopened"      -> pure PullRequestReopenedAction
        _               -> pure (PullRequestActionOther t)

-- | Triggered when a pull request is assigned, unassigned, labeled, unlabeled, opened, edited,
-- closed, reopened, or synchronized. Also triggered when a pull request review is requested,
-- or when a review request is removed.
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestevent>.
data PullRequestEvent = PullRequestEvent
    { evPullReqAction           :: !PullRequestEventAction
    , evPullReqNumber           :: !Int
    , evPullReqPayload          :: !HookPullRequest
    , evPullReqRepo             :: !HookRepository
    , evPullReqSender           :: !HookUser
    , evPullReqInstallationId   :: !(Maybe Int)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PullRequestEvent where senderOfEvent = evPullReqSender
instance EventHasRepo PullRequestEvent where repoForEvent = evPullReqRepo
instance NFData PullRequestEvent where rnf = genericRnf


data PullRequestReviewEventAction
  -- | Decodes from "submitted"
  = PullRequestReviewSubmittedAction
  -- | Decodes from "edited"
  | PullRequestReviewEditedAction
  -- | Decodes from "dismissed"
  | PullRequestReviewDismissedAction
  -- | The result of decoding an unknown pull request review event action type
  | PullRequestReviewActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestReviewEventAction where rnf = genericRnf

instance FromJSON PullRequestReviewEventAction where
  parseJSON = withText "Pull request review event action" $ \t ->
    case t of
        "submitted"     -> pure PullRequestReviewSubmittedAction
        "edited"        -> pure PullRequestReviewEditedAction
        "dismissed"     -> pure PullRequestReviewDismissedAction
        _               -> pure (PullRequestReviewActionOther t)

-- | Triggered when a pull request review is submitted into a non-pending state,
-- the body is edited, or the review is dismissed.
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestreviewevent>.
data PullRequestReviewEvent = PullRequestReviewEvent
    { evPullReqReviewAction     :: !PullRequestReviewEventAction
    , evPullReqReviewPayload    :: !HookPullRequestReview
    , evPullReqReviewTarget     :: !HookPullRequest
    , evPullReqReviewRepo       :: !HookRepository
    , evPullReqReviewSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PullRequestReviewEvent where senderOfEvent = evPullReqReviewSender
instance EventHasRepo PullRequestReviewEvent where repoForEvent = evPullReqReviewRepo
instance NFData PullRequestReviewEvent where rnf = genericRnf


data PullRequestReviewCommentEventAction
  -- | Decodes from "created"
  = PullRequestReviewCommentCreatedAction
  -- | Decodes from "edited"
  | PullRequestReviewCommentEditedAction
  -- | Decodes from "deleted"
  | PullRequestReviewCommentDeletedAction
  -- | The result of decoding an unknown pull request review comment event action type
  | PullRequestReviewCommentActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestReviewCommentEventAction where rnf = genericRnf

instance FromJSON PullRequestReviewCommentEventAction where
  parseJSON = withText "Pull request review comment event action" $ \t ->
    case t of
        "created"       -> pure PullRequestReviewCommentCreatedAction
        "edited"        -> pure PullRequestReviewCommentEditedAction
        "deleted"       -> pure PullRequestReviewCommentDeletedAction
        _               -> pure (PullRequestReviewCommentActionOther t)

-- | Triggered when a comment on a pull request's unified diff is created,
-- edited, or deleted (in the Files Changed tab).
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestreviewcommentevent>.
data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { evPullReqRevComAction     :: !PullRequestReviewCommentEventAction
    , evPullReqRevComment       :: !HookPullRequestReviewComment
    , evPullReqRevTarget        :: !HookPullRequest
    , evPullReqRevRepo          :: !HookRepository
    , evPullReqRevSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PullRequestReviewCommentEvent where senderOfEvent = evPullReqRevSender
instance EventHasRepo PullRequestReviewCommentEvent where repoForEvent = evPullReqRevRepo
instance NFData PullRequestReviewCommentEvent where rnf = genericRnf


-- | Triggered on a push to a repository branch. Branch pushes and repository tag
-- pushes also trigger webhook push events.
-- See <https://developer.github.com/v3/activity/events/types/#pushevent>.
data PushEvent = PushEvent
    { evPushRef                 :: !Text
    , evPushHeadSha             :: !(Maybe Text)
    , evPushBeforeSha           :: !(Maybe Text)
    , evPushCreated             :: !Bool
    , evPushDeleted             :: !Bool
    , evPushForced              :: !Bool
    , evPushBaseRef             :: !(Maybe Text)
    , evPushCompareUrl          :: !URL
    , evPushCommits             :: !(Maybe (Vector HookCommit))
    , evPushHeadCommit          :: !(Maybe HookCommit)
    , evPushRepository          :: !HookRepository
    , evPushOrganization        :: !(Maybe HookOrganization)
    , evPushSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender PushEvent where senderOfEvent = evPushSender
instance EventHasRepo PushEvent where repoForEvent = evPushRepository
instance NFData PushEvent where rnf = genericRnf


data ReleaseEventAction
  -- | Decodes from "published"
  = ReleasePublishedAction
  -- | The result of decoding an unknown release event action type
  | ReleaseActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData ReleaseEventAction where rnf = genericRnf

instance FromJSON ReleaseEventAction where
  parseJSON = withText "Release event action" $ \t ->
    case t of
        "published"     -> pure ReleasePublishedAction
        _               -> pure (ReleaseActionOther t)

-- | Triggered when a release is published.
-- See <https://developer.github.com/v3/activity/events/types/#releaseevent>.
data ReleaseEvent = ReleaseEvent
    { evReleaseEventAction      :: !ReleaseEventAction      -- ^ Currently only releasePublished.
    , evReleaseEventPayload     :: !HookRelease
    , evReleaseEventRepo        :: !HookRepository
    , evReleaseEventSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender ReleaseEvent where senderOfEvent = evReleaseEventSender
instance EventHasRepo ReleaseEvent where repoForEvent = evReleaseEventRepo
instance NFData ReleaseEvent where rnf = genericRnf


data RepositoryEventAction
  -- | Decodes from "created"
  = RepositoryCreatedAction
  -- | Decodes from "deleted"
  | RepositoryDeletedAction
  -- | Decodes from "archived"
  | RepositoryArchivedAction
  -- | Decodes from "unarchived"
  | RepositoryUnarchivedAction
  -- | Decodes from "publicized"
  | RepositoryPublicizedAction
  -- | Decodes from "privatized"
  | RepositoryPrivatizedAction
  -- | The result of decoding an unknown repository event action type
  | RepositoryActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData RepositoryEventAction where rnf = genericRnf

instance FromJSON RepositoryEventAction where
  parseJSON = withText "Repository event action" $ \t ->
    case t of
        "created"       -> pure RepositoryCreatedAction
        "deleted"       -> pure RepositoryDeletedAction
        "archived"      -> pure RepositoryArchivedAction
        "unarchived"    -> pure RepositoryUnarchivedAction
        "publicized"    -> pure RepositoryPublicizedAction
        "privatized"    -> pure RepositoryPrivatizedAction
        _               -> pure (RepositoryActionOther t)

-- | Triggered when a repository is created, archived, unarchived, made public, or made private.
-- Organization hooks are also triggered when a repository is deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#repositoryevent>.
data RepositoryEvent = RepositoryEvent
    { evRepositoryAction        :: !RepositoryEventAction
    , evRepositoryTarget        :: !HookRepository
    , evRepositoryOrg           :: !(Maybe HookOrganization)
    , evRepositorySender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender RepositoryEvent where senderOfEvent = evRepositorySender
instance EventHasRepo RepositoryEvent where repoForEvent = evRepositoryTarget
instance NFData RepositoryEvent where rnf = genericRnf


data StatusEventState
  -- | Decodes from "pending"
  = StatusPendingState
  -- | Decodes from "success"
  | StatusSuccessState
  -- | Decodes from "failure"
  | StatusFailureState
  -- | Decodes from "error"
  | StatusErrorState
  -- | The result of decoding an unknown status event state
  | StatusStateOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData StatusEventState where rnf = genericRnf

instance FromJSON StatusEventState where
  parseJSON = withText "Status event state" $ \t ->
    case t of
        "pending"       -> pure StatusPendingState
        "success"       -> pure StatusSuccessState
        "failure"       -> pure StatusFailureState
        "error"         -> pure StatusErrorState
        _               -> pure (StatusStateOther t)

-- | Triggered when the status of a Git commit changes.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#statusevent>.
data StatusEvent = StatusEvent
    { evStatusId                :: !Int
    , evStatusCommitSha         :: !Text
    , evStatusCommitName        :: !Text
    , evStatusTargetUrl         :: !(Maybe URL)
    , evStatusContext           :: !Text
    , evStatusDescription       :: !(Maybe Text)
    , evStatusState             :: !StatusEventState
    , evStatusCommit            :: !HookCommit
    -- FIXME: Branches are missing here
    , evStatusCreatedAt         :: !UTCTime
    , evStatusUpdatedAt         :: !UTCTime
    , evStatusRepo              :: !HookRepository
    , evStatusSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender StatusEvent where senderOfEvent = evStatusSender
instance EventHasRepo StatusEvent where repoForEvent = evStatusRepo
instance NFData StatusEvent where rnf = genericRnf


data TeamEventAction
  -- | Decodes from "created"
  = TeamCreatedAction
  -- | Decodes from "deleted"
  | TeamDeletedAction
  -- | Decodes from "edited"
  | TeamEditedAction
  -- | Decodes from "added_to_repository"
  | TeamAddedToRepoAction
  -- | Decodes from "removed_from_repository"
  | TeamRemovedFromRepoAction
  -- | The result of decoding an unknown team event action type
  | TeamActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData TeamEventAction where rnf = genericRnf

instance FromJSON TeamEventAction where
  parseJSON = withText "Team event action" $ \t ->
    case t of
        "created"       -> pure TeamCreatedAction
        "deleted"       -> pure TeamDeletedAction
        "edited"        -> pure TeamEditedAction
        "added_to_repository"     -> pure TeamAddedToRepoAction
        "removed_from_repository" -> pure TeamRemovedFromRepoAction
        _               -> pure (TeamActionOther t)

-- | Triggered when an organization's team is created or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger organization hooks.
-- See <https://developer.github.com/v3/activity/events/types/#teamevent>.
data TeamEvent = TeamEvent
    { evTeamAction              :: !TeamEventAction
    , evTeamTarget              :: !HookTeam
    , evTeamOrganization        :: !HookOrganization
    , evTeamSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender TeamEvent where senderOfEvent = evTeamSender
instance NFData TeamEvent where rnf = genericRnf


-- | Triggered when a repository is added to a team.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#teamaddevent>.
data TeamAddEvent = TeamAddEvent
    { evTeamAddTarget           :: !(Maybe HookTeam) -- ^ Older events may not include this in the payload.
    , evTeamAddRepo             :: !HookRepository
    , evTeamAddOrg              :: !HookOrganization
    , evTeamAddSender           :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender TeamAddEvent where senderOfEvent = evTeamAddSender
instance EventHasRepo TeamAddEvent where repoForEvent = evTeamAddRepo
instance NFData TeamAddEvent where rnf = genericRnf


data WatchEventAction
  -- | Decodes from "started"
  = WatchStartedAction
  -- | The result of decoding an unknown watch event action type
  | WatchActionOther !Text
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData WatchEventAction where rnf = genericRnf

instance FromJSON WatchEventAction where
  parseJSON = withText "Watch event action" $ \t ->
    case t of
        "started"       -> pure WatchStartedAction
        _               -> pure (WatchActionOther t)

-- | The WatchEvent is related to starring a repository, not watching.
-- The event’s actor is the user who starred a repository, and the event’s
-- repository is the repository that was starred.
-- See <https://developer.github.com/v3/activity/events/types/#watchevent>.
data WatchEvent = WatchEvent
    { evWatchAction             :: !WatchEventAction
    , evWatchRepo               :: !HookRepository
    , evWatchSender             :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance EventHasSender WatchEvent where senderOfEvent = evWatchSender
instance EventHasRepo WatchEvent where repoForEvent = evWatchRepo
instance NFData WatchEvent where rnf = genericRnf


-- Aeson Instances

instance FromJSON CheckSuiteEvent where
    parseJSON = withObject "CheckSuiteEvent" $ \o -> CheckSuiteEvent
        <$> o .: "action"
        <*> o .: "check_suite"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"
        <*> o .:? "installation"

instance FromJSON CheckRunEvent where
    parseJSON = withObject "CheckRunEvent" $ \o -> CheckRunEvent
        <$> o .: "action"
        <*> o .: "check_run"
        <*> o .:? "requested_action"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"
        <*> o .:? "installation"

instance FromJSON CommitCommentEvent where
    parseJSON = withObject "CommitCommentEvent" $ \o -> CommitCommentEvent
        <$> o .: "action"
        <*> o .: "comment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON CreateEvent where
    parseJSON = withObject "CreateEvent" $ \o -> CreateEvent
        <$> o .: "ref"
        <*> o .: "ref_type"
        <*> o .: "master_branch"
        <*> o .: "description"
        <*> o .: "pusher_type"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeleteEvent where
    parseJSON = withObject "DeleteEvent" $ \o -> DeleteEvent
        <$> o .: "ref"
        <*> o .: "ref_type"
        <*> o .: "pusher_type"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeploymentEvent where
    parseJSON = withObject "DeploymentEvent" $ \o -> DeploymentEvent
        <$> o .: "deployment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeploymentStatusEvent where
    parseJSON = withObject "DeploymentStatusEvent" $ \o -> DeploymentStatusEvent
        <$> o .: "deployment_status"
        <*> o .: "deployment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON ForkEvent where
    parseJSON = withObject "ForkEvent" $ \o -> ForkEvent
        <$> o .: "forkee"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON GollumEvent where
    parseJSON = withObject "GollumEvent" $ \o -> GollumEvent
        <$> o .: "pages"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON InstallationEvent where
    parseJSON = withObject "InstallationEvent" $ \o -> InstallationEvent
        <$> o .: "action"
        <*> o .: "installation"
        <*> o .:? "repositories" .!= mempty
        <*> o .: "sender"

instance FromJSON InstallationRepositoriesEvent where
    parseJSON = withObject "InstallationRepositoriesEvent" $ \o -> InstallationRepositoriesEvent
        <$> o .: "action"
        <*> o .: "installation"
        <*> o .: "repository_selection"
        <*> o .: "repositories_added"
        <*> o .: "repositories_removed"
        <*> o .: "sender"

instance FromJSON IssueCommentEvent where
    parseJSON = withObject "IssueCommentEvent" $ \o -> IssueCommentEvent
        <$> o .: "action"
        <*> o .: "issue"
        <*> o .: "comment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON IssuesEvent where
    parseJSON = withObject "IssuesEvent" $ \o -> IssuesEvent
        <$> o .: "action"
        <*> o .: "issue"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON LabelEvent where
    parseJSON = withObject "LabelEvent" $ \o -> LabelEvent
        <$> o .: "action"
        <*> o .: "label"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"

instance FromJSON MemberEvent where
    parseJSON = withObject "MemberEvent" $ \o -> MemberEvent
        <$> o .: "action"
        <*> o .: "member"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON MembershipEvent where
    parseJSON = withObject "MembershipEvent" $ \o -> MembershipEvent
        <$> o .: "action"
        <*> o .: "scope"
        <*> o .: "member"
        <*> o .: "team"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON MilestoneEvent where
    parseJSON = withObject "MilestoneEvent" $ \o -> MilestoneEvent
        <$> o .: "action"
        <*> o .: "milestone"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON OrganizationEvent where
    parseJSON = withObject "OrganizationEvent" $ \o -> OrganizationEvent
        <$> o .: "action"
        <*> o .: "invitation"
        <*> o .: "membership"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON OrgBlockEvent where
    parseJSON = withObject "OrgBlockEvent" $ \o -> OrgBlockEvent
        <$> o .: "action"
        <*> o .: "blocked_user"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON PageBuildEvent where
    parseJSON = withObject "PageBuildEvent" $ \o -> PageBuildEvent
        <$> o .: "id"
        <*> o .: "build"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON ProjectCardEvent where
    parseJSON = withObject "ProjectCardEvent" $ \o -> ProjectCardEvent
        <$> o .: "action"
        <*> o .: "project_card"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON ProjectColumnEvent where
    parseJSON = withObject "ProjectColumnEvent" $ \o -> ProjectColumnEvent
        <$> o .: "action"
        <*> o .: "project_column"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON ProjectEvent where
    parseJSON = withObject "ProjectEvent" $ \o -> ProjectEvent
        <$> o .: "action"
        <*> o .: "project"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON PublicEvent where
    parseJSON = withObject "PublicEvent" $ \o -> PublicEvent
        <$> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestEvent where
    parseJSON = withObject "PullRequestEvent" $ \o -> PullRequestEvent
        <$> o .: "action"
        <*> o .: "number"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"
        <*> (o .:? "installation" >>= maybe (pure Nothing) (.:? "id"))

instance FromJSON PullRequestReviewEvent where
    parseJSON = withObject "PullRequestReviewEvent" $ \o -> PullRequestReviewEvent
        <$> o .: "action"
        <*> o .: "review"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON = withObject "PullRequestReviewCommentEvent" $ \o -> PullRequestReviewCommentEvent
        <$> o .: "action"
        <*> o .: "comment"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PushEvent where
    parseJSON = withObject "PushEvent" $ \o -> PushEvent
        <$> o .: "ref"
        <*> o .:? "after"
        <*> o .:? "before"
        <*> o .: "created"
        <*> o .: "deleted"
        <*> o .: "forced"
        <*> o .:? "base_ref"
        <*> o .: "compare"
        <*> o .:? "commits"
        <*> o .:? "head_commit"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"

instance FromJSON ReleaseEvent where
    parseJSON = withObject "ReleaseEvent" $ \o -> ReleaseEvent
        <$> o .: "action"
        <*> o .: "release"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON RepositoryEvent where
    parseJSON = withObject "RepositoryEvent" $ \o -> RepositoryEvent
        <$> o .: "action"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON StatusEvent where
    parseJSON = withObject "StatusEvent" $ \o -> StatusEvent
        <$> o .: "id"
        <*> o .: "sha"
        <*> o .: "name"
        <*> o .:? "target_url"
        <*> o .: "context"
        <*> o .:? "description"
        <*> o .: "state"
        <*> o .: "commit"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON TeamEvent where
    parseJSON = withObject "TeamEvent" $ \o -> TeamEvent
        <$> o .: "action"
        <*> o .: "team"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON TeamAddEvent where
    parseJSON = withObject "TeamAddEvent" $ \o -> TeamAddEvent
        <$> o .:? "team"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON WatchEvent where
    parseJSON = withObject "WatchEvent" $ \o -> WatchEvent
        <$> o .: "action"
        <*> o .: "repository"
        <*> o .: "sender"
