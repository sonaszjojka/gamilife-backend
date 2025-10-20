package edu.pjwstk.groups.util;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.groups.entity.GroupInvitation;

import java.time.LocalDateTime;
import java.util.UUID;

import java.time.LocalDateTime;
import java.util.UUID;

public interface GroupInvitationUtil {
    String generateToken();

    String generateGroupInvitationLink(UUID groupId, UUID groupInvitationId, String token);

    LocalDateTime calculateExpirationDate();

    String hashToken(String token);

    String generateInvitationMailContentMessage(String link, String joinCode);

    String generateInvitationMailSubjectMessage();

    boolean verifyToken(String token, String hashedTokenFromDb);

}


