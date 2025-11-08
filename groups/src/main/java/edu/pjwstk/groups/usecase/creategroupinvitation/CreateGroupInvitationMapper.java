package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.InvitationStatus;

import java.time.LocalDateTime;
import java.util.UUID;

public interface CreateGroupInvitationMapper {

    GroupInvitation toEntity(Group groupInvited, InvitationStatus invitationStatus, UUID userId,
                             LocalDateTime expiresAt, String link, UUID groupInvitationId, String tokenHash);

    CreateGroupInvitationResponse toResponse(GroupInvitation savedGroupInvitation);
}
