package edu.pjwstk.groups.util.impl;

import edu.pjwstk.groups.util.GroupInvitationUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Base64;
import java.util.UUID;

@Component
public class GroupInvitationUtilImpl implements GroupInvitationUtil {

    private static final SecureRandom secureRandom = new SecureRandom();
    @Value("${app.invitation.expiration-days}")
    private int groupInvitationExpirationDays;
    @Value("${app.invitation.invitation-url-prefix}")
    private String invitationUrlPrefix;

    @Override
    public String generateGroupInvitationLink(UUID groupId, UUID groupInvitationId, String token) {
        return invitationUrlPrefix + "/app/groups/" + groupId + "/group-invitations/" + groupInvitationId + "?token=" + token;
    }

    @Override
    public LocalDateTime calculateExpirationDate() {
        return LocalDateTime.now().plusDays(groupInvitationExpirationDays);
    }

    @Override
    public String generateToken() {
        byte[] randomBytes = new byte[24];
        secureRandom.nextBytes(randomBytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(randomBytes);
    }

    @Override
    public String hashToken(String token) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(token.getBytes(StandardCharsets.UTF_8));
            return Base64.getUrlEncoder().encodeToString(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 algorithm not available", e);
        }
    }

    @Override
    public String generateInvitationMailContentMessage(String link, String joinCode) {
        return String.format(
                "<html>" +
                        "<body>" +
                        "<p>You have been invited to join the group with code: <b>%s</b>.</p>" +
                        "<p>Click the link below to open the invitation page and decide whether to join:</p>" +
                        "<p><a href=\"%s\">Join the group</a></p>" +
                        "</body>" +
                        "</html>",
                joinCode, link
        );
    }


    @Override
    public String generateInvitationMailSubjectMessage() {
        return "Group Invitation";
    }

    @Override
    public boolean verifyToken(String token, String hashedTokenFromDb) {
        String hashedToken = hashToken(token);
        return hashedToken.equals(hashedTokenFromDb);
    }
}


