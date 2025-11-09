package edu.pjwstk.groups.model;

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

    @Column(name = "joined_at", nullable = false)
    protected Instant joinedAt;
    @Column(name = "left_at")
    protected Instant leftAt;
    @Id
    @Column(name = "group_member_id", nullable = false, updatable = false, unique = true)
    private UUID groupMemberId;
    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group memberGroup;
    @Column(name = "user_id", updatable = false, nullable = false)
    private UUID userId;
    @Column(name = "group_money", nullable = false)
    private Integer groupMoney;
    @Column(name = "total_earned_money", nullable = false)
    private Integer totalEarnedMoney;
    @OneToMany(mappedBy = "senderGroupMember")
    @ToString.Exclude
    private List<ChatMessage> chatMessages;

    @PrePersist
    public void prePersist() {
        this.joinedAt = Instant.now();
    }

    public boolean isActive() {
        return this.leftAt == null;
    }

    public boolean isUser(UUID userId) {
        return this.userId.equals(userId);
    }
}
