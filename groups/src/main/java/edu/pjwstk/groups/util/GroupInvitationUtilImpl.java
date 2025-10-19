package edu.pjwstk.groups.util;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class GroupInvitationUtilImpl implements GroupInvitationUtil {

    @Value("${app.invitation.expiration-days:14}")
    private int groupInvitationExpirationDays;

    @Value("${app.invitation.invitation-url-prefix}")
    private String invitationUrlPrefix;

    @Override
    public String generateGroupInvitationLink(UUID groupId, UUID groupInvitationId) {
        return invitationUrlPrefix + "api/v1/groups/" + groupId + "/group-invitations/" + groupInvitationId;
    }

    @Override
    public LocalDateTime calculateExpirationDate() {
        return LocalDateTime.now().plusDays(groupInvitationExpirationDays);
    }
}
