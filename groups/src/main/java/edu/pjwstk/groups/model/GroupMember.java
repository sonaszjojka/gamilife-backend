package edu.pjwstk.groups.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
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
    @Column(name = "group_id", updatable = false, nullable = false, insertable = false)
    private UUID groupId;
    @ToString.Exclude
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group group;
    @Column(name = "user_id", updatable = false, nullable = false)
    private UUID userId;
    @Column(name = "group_money", nullable = false)
    private Integer groupMoney;
    @Column(name = "total_earned_money", nullable = false)
    private Integer totalEarnedMoney;
    @OneToMany(mappedBy = "groupMember", fetch = FetchType.LAZY)
    @ToString.Exclude
    @Builder.Default
    private Set<ChatMessage> chatMessages = new HashSet<>();

    @PrePersist
    public void prePersist() {
        if (this.joinedAt == null && this.leftAt != null) {
            this.joinedAt = Instant.now();
        }
    }

    public boolean isActive() {
        return this.leftAt == null;
    }

    public boolean isUser(UUID userId) {
        return this.userId.equals(userId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        GroupMember that = (GroupMember) o;
        return Objects.equals(groupMemberId, that.groupMemberId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupMemberId);
    }
}
