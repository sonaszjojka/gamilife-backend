package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.group.enums.InvitationStatusEnum;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_invitation")
public class GroupInvitation {

    @Id
    @Column(name = "group_invitation_id", nullable = false, updatable = false, unique = true)
    private UUID groupInvitationId;

    @Column(name = "group_id", nullable = false, updatable = false, insertable = false)
    private UUID groupId;

    @ManyToOne(fetch = FetchType.LAZY)
    @ToString.Exclude
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group group;

    @Column(name = "user_id", nullable = false, updatable = false)
    private UUID userId;

    @Column(name = "expires_at", nullable = false)
    private LocalDateTime expiresAt;

    @Column(name = "mail_sent_at")
    private LocalDateTime mailSentAt;

    @Column(name = "link", nullable = false, length = 200)
    private String link;

    @Column(name = "token_hash", nullable = false)
    private String tokenHash;

    @Column(name = "invitation_status_id", nullable = false, insertable = false, updatable = false)
    private Integer invitationStatusId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "invitation_status_id", nullable = false)
    private InvitationStatus invitationStatus;

    public boolean doesBelongToUser(UUID userId) {
        return this.userId.equals(userId);
    }

    public boolean isExpired() {
        return LocalDateTime.now().isAfter(this.expiresAt);
    }

    public boolean hasStatus(InvitationStatusEnum statusEnum) {
        return this.invitationStatus.toEnum() == statusEnum;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        GroupInvitation that = (GroupInvitation) o;
        return Objects.equals(groupInvitationId, that.groupInvitationId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupInvitationId);
    }
}
