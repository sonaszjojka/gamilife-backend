package edu.pjwstk.groups.util;

import java.time.LocalDateTime;
import java.util.UUID;

import java.time.LocalDateTime;
import java.util.UUID;

public interface GroupInvitationUtil {
    String generateToken();

    String generateGroupInvitationLink(UUID groupId, UUID groupInvitationId, String token);

    LocalDateTime calculateExpirationDate();

    String hashToken(String token);

    boolean verifyToken(String token, String hashedTokenFromDb);
}


