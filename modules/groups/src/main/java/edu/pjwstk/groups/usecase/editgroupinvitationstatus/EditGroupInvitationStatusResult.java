package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.GroupInvitation}
 */
@Builder
public record EditGroupInvitationStatusResult(UUID groupInvitationId, GroupDto groupInvited, UUID userId,
                                              LocalDateTime expiresAt, LocalDateTime mailSentAt, String link,
                                              InvitationStatusDto invitationStatus,
                                              UUID groupMemberId) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.model.InvitationStatus}
     */
    @Builder
    public record InvitationStatusDto(Integer invitationStatusId, String title) implements Serializable {
    }
}