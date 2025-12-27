package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Base64;
import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_invitation")
public class GroupInvitation extends BaseEntity {

    @Column(name = "group_id", nullable = false, updatable = false, insertable = false)
    private UUID groupId;

    @ManyToOne(fetch = FetchType.LAZY)
    @ToString.Exclude
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group group;

    @Column(name = "user_id", nullable = false, updatable = false)
    private UUID userId;

    @Column(name = "expires_at", nullable = false)
    private Instant expiresAt;

    @Column(name = "link", nullable = false, length = 200)
    private String link;

    @Column(name = "token_hash", nullable = false)
    private String tokenHash;

    @Column(name = "status_id", nullable = false, insertable = false, updatable = false)
    private Integer statusId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "status_id", nullable = false)
    private InvitationStatus status;

    private GroupInvitation(Group group, UUID userId, int expiresInDays, String token, String invitationUrlPrefix, InvitationStatus invitationStatus) {
        setGroup(group);
        setUserId(userId);
        setTokenHash(token);
        setLink(invitationUrlPrefix, token);
        setExpirationDate(expiresInDays);
        changeStatus(invitationStatus);
    }

    public static GroupInvitation create(Group group, UUID userId, int expiresInDays, String invitationUrlPrefix, InvitationStatus invitationStatus) {
        String token = generateToken();
        return new GroupInvitation(group, userId, expiresInDays, token, invitationUrlPrefix, invitationStatus);
    }

    private static String generateToken() {
        byte[] randomBytes = new byte[24];
        new SecureRandom().nextBytes(randomBytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(randomBytes);
    }

    public boolean doesBelongToUser(UUID userId) {
        return this.userId.equals(userId);
    }

    private static String hashToken(String token) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(token.getBytes(StandardCharsets.UTF_8));
            return Base64.getUrlEncoder().encodeToString(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 algorithm not available", e);
        }
    }

    public void changeStatus(InvitationStatus invitationStatus) {
        if (invitationStatus == null) {
            throw new DomainValidationException("Invitation status cannot be null");
        }

        this.status = invitationStatus;
        this.statusId = invitationStatus.getId();
    }

    public boolean isExpired() {
        return Instant.now().isAfter(this.expiresAt);
    }

    public boolean hasStatus(InvitationStatusEnum statusEnum) {
        return this.status.toEnum() == statusEnum;
    }

    public boolean verifyToken(String token) {
        String hashedToken = hashToken(token);
        return hashedToken.equals(this.tokenHash);
    }

    private void setLink(String invitationUrlPrefix, String token) {
        this.link = invitationUrlPrefix + "/app/groups/" + groupId + "/group-invitations/" + getId() + "?token=" + token;
    }

    private void setGroup(Group group) {
        if (group == null) {
            throw new DomainValidationException("Group cannot be null");
        }

        this.group = group;
        this.groupId = group.getId();
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User id cannot be null");
        }

        this.userId = userId;
    }

    private void setExpirationDate(int expiresInDays) {
        this.expiresAt = Instant.now().plus(expiresInDays, ChronoUnit.DAYS);
    }

    private void setTokenHash(String token) {
        if (token == null || token.isBlank()) {
            throw new DomainValidationException("Token cannot be null or empty");
        }

        this.tokenHash = hashToken(token);
    }

}
