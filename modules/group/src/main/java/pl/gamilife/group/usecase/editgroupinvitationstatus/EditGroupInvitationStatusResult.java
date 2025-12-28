package pl.gamilife.group.usecase.editgroupinvitationstatus;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.InvitationStatus;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link GroupInvitation}
 */
@Builder
public record EditGroupInvitationStatusResult(UUID groupInvitationId, GroupDto groupInvited, UUID userId,
                                              Instant expiresAt, Instant mailSentAt, String link,
                                              InvitationStatusDto invitationStatus,
                                              UUID groupMemberId) implements Serializable {
    /**
     * DTO for {@link Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link InvitationStatus}
     */
    @Builder
    public record InvitationStatusDto(Integer invitationStatusId, String title) implements Serializable {
    }
}