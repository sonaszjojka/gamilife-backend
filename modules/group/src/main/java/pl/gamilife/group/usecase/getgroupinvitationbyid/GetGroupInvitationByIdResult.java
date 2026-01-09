package pl.gamilife.group.usecase.getgroupinvitationbyid;

import java.util.UUID;

public record GetGroupInvitationByIdResult(
        UUID id,
        UUID groupId,
        String groupName,
        UUID invitedUserId,
        InvitationStatus status,
        long expiresAt
) {
    public enum InvitationStatus {
        SENT, ACCEPTED, DECLINED, REVOKED
    }
}
