package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.group.exception.domain.NotEnoughGroupMoneyException;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_member", schema = "group")
public class GroupMember extends BaseEntity {

    @Column(name = "group_id", updatable = false, nullable = false, insertable = false)
    private UUID groupId;

    @ToString.Exclude
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group group;

    @Column(name = "user_id", updatable = false, nullable = false)
    private UUID userId;

    @Column(name = "joined_at", nullable = false)
    protected Instant joinedAt = Instant.now();

    @Column(name = "left_at")
    protected Instant leftAt;

    @Column(name = "group_money", nullable = false)
    private Integer groupMoney = 0;

    @Column(name = "total_earned_money", nullable = false)
    private Integer totalEarnedMoney = 0;

    @OneToMany(mappedBy = "groupMember", fetch = FetchType.LAZY)
    @ToString.Exclude
    private final Set<ChatMessage> chatMessages = new HashSet<>();

    private GroupMember(Group group, UUID userId) {
        setGroup(group);
        setUserId(userId);
    }

    public static GroupMember create(Group group, UUID userId) {
        return new GroupMember(group, userId);
    }

    public void leave() {
        if (leftAt != null) {
            throw new IllegalStateException("User has already left the group");
        }

        this.leftAt = Instant.now();
    }

    public void rejoin() {
        if (leftAt == null) {
            throw new IllegalStateException("User has not left the group");
        }

        this.leftAt = null;
        this.joinedAt = Instant.now();
    }

    public boolean isActive() {
        return this.leftAt == null;
    }

    public boolean isUser(UUID userId) {
        return this.userId.equals(userId);
    }

    public void gainMoney(int amount) {
        if (amount <= 0) {
            throw new DomainValidationException("Amount cannot be less than or equal to 0");
        }

        this.groupMoney += amount;
        this.totalEarnedMoney += amount;
    }

    public void useMoney(int amount) {
        if (amount <= 0) {
            throw new DomainValidationException("Amount cannot be less than or equal to 0");
        }

        if (this.groupMoney < amount) {
            throw new DomainValidationException("Not enough money to use");
        }

        this.groupMoney -= amount;
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

    public void setGroupMoney(Integer groupMoney) {
        if (groupMoney == null) {
            throw new DomainValidationException("Group money cannot be null");
        }

        if (groupMoney < 0) {
            throw new DomainValidationException("Group money cannot be negative");
        }

        this.totalEarnedMoney = this.totalEarnedMoney - this.groupMoney + groupMoney;
        this.groupMoney = groupMoney;
    }

    public boolean isAdmin() {
        return this.group.getAdminId().equals(this.getUserId());
    }

    public void payMoney(Integer amount) {
        if (amount <= 0) {
            throw new DomainValidationException("Amount cannot be less than or equal to 0");
        }

        if (this.groupMoney < amount) {
            throw new NotEnoughGroupMoneyException("Not enough money to pay");
        }

        this.groupMoney -= amount;
    }
}
