package edu.pjwstk.groups.usecase.creategroupinvitation;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.GroupInvitation}
 */
@Builder
public record CreateGroupInvitationResponse(UUID groupInvitationId,
                                            CreateGroupInvitationResponse.GroupDto groupInvited,
                                            UUID userId,
                                            LocalDateTime expiresAt, LocalDateTime mailSentAt, String link,
                                            CreateGroupInvitationResponse.InvitationStatusDto invitationStatus)
        implements Serializable {
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
