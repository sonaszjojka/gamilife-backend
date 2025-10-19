package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.entity.GroupInvitation}
 */
@Builder
public record EditGroupInvitationStatusResponse(UUID groupInvitationId, GroupDto groupInvited, UUID userId,
                                                LocalDateTime expiresAt, LocalDateTime mailSentAt, String link,
                                                InvitationStatusDto invitationStatus,
                                                CreateGroupMemberResponse groupMemberResponse) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.entity.Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.entity.InvitationStatus}
     */
    @Builder
    public record InvitationStatusDto(Integer invitationStatusId, String title) implements Serializable {
    }
}