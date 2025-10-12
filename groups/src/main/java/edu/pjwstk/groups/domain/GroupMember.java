package edu.pjwstk.groups.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_member")
public class GroupMember {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "group_member_id", nullable = false, updatable = false, unique = true)
    private Integer groupMemberId;

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group memberGroup;

    @Column(name = "user_id", updatable = false, nullable = false)
    private UUID userId;

    @Column(name = "joined_at", nullable = false)
    protected Instant joinedAt;

    @PrePersist
    public void prePersist() {
        this.joinedAt = Instant.now();
    }

    @Column(name = "left_at", nullable = true)
    protected Instant leftAt;

    @Column(name = "group_money", nullable = false)
    private Integer groupMoney;

    @Column(name = "total_earned_money", nullable = false)
    private Integer totalEarnedMoney;

    @OneToMany(mappedBy = "senderGroupMember")
    @ToString.Exclude
    private List<ChatMessage> chatMessages;
}
