package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.InvitationStatusEnum;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
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

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group groupInvited;

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

    @ManyToOne
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

    public boolean doesBelongToGroup(UUID groupId) {
        return this.groupInvited.getGroupId().equals(groupId);
    }
}
