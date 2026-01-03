package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_invitation", schema = "group")
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

    @Column(name = "token_hash", nullable = false)
    private String tokenHash;

    @Column(name = "status_id", nullable = false, insertable = false, updatable = false)
    private Integer statusId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "status_id", nullable = false)
    private InvitationStatus status;

    private GroupInvitation(Group group, UUID userId, int expiresInDays, String token, InvitationStatus invitationStatus) {
        setGroup(group);
        setUserId(userId);
        setTokenHash(token);
        setExpirationDate(expiresInDays);
        changeStatus(invitationStatus);
    }

    public static GroupInvitation create(Group group, UUID userId, int expiresInDays, String hashedToken, InvitationStatus invitationStatus) {
        return new GroupInvitation(group, userId, expiresInDays, hashedToken, invitationStatus);
    }

    public boolean doesBelongToUser(UUID userId) {
        return this.userId.equals(userId);
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

    private void setTokenHash(String tokenHash) {
        if (tokenHash == null || tokenHash.isBlank()) {
            throw new DomainValidationException("Token cannot be null or empty");
        }

        this.tokenHash = tokenHash;
    }

}
