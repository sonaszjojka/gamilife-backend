package pl.gamilife.group.util;

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


