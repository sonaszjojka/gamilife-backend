package edu.pjwstk.groups.util;

import java.time.LocalDateTime;
import java.util.UUID;

public interface GroupInvitationUtil {
    String generateGroupInvitationLink(UUID groupId, UUID groupInvitationId);

    LocalDateTime calculateExpirationDate();
}
