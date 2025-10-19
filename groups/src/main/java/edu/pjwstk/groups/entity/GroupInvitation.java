package edu.pjwstk.groups.entity;

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

    @ManyToOne
    @JoinColumn(name = "invitation_status_id", nullable = false)
    private InvitationStatus invitationStatus;
}
