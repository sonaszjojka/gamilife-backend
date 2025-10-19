package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.entity.InvitationStatus;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusResponse;

import java.time.LocalDateTime;
import java.util.UUID;

public interface CreateGroupInvitationStatusMapper {

    GroupInvitation toEntity(Group groupInvited, InvitationStatus invitationStatus, UUID userId,
                             LocalDateTime expiresAt, String link, UUID groupInvitationId);

    CreateGroupInvitationResponse toResponse(GroupInvitation savedGroupInvitation);
}
