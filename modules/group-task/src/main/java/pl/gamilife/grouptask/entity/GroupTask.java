package pl.gamilife.grouptask.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
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
@Table(name = "group_task")
public class GroupTask extends BaseEntity {

    @NotNull
    @Column(name = "task_id", nullable = false)
    private UUID taskId;

    @NotNull
    @Column(name = "group_id", nullable = false)
    private UUID groupId;

    @Column(name = "reward")
    private Integer reward;

    @Size(max = 300)
    @Column(name = "decline_message", length = 300)
    private String declineMessage;

    @Column(name = "accepted_at")
    private Instant acceptedAt;

    @Column(name = "reward_issued_at")
    private Instant rewardIssuedAt;

    @OneToMany(mappedBy = "groupTask", cascade = CascadeType.ALL, orphanRemoval = true)
    @ToString.Exclude
    private final Set<GroupTaskMember> groupTaskMembers = new HashSet<>();

    private GroupTask(UUID taskId, UUID groupId, Integer reward) {
        setTaskId(taskId);
        setGroupId(groupId);
        setReward(reward);
    }

    public static GroupTask create(UUID taskId, UUID groupId, Integer reward) {
        return new GroupTask(taskId, groupId, reward);
    }

    public boolean isAccepted() {
        return this.acceptedAt != null;
    }

    public void setDeclineMessage(String declineMessage) {
        if (declineMessage == null || declineMessage.isBlank()) {
            this.declineMessage = null;
            return;
        }

        if (declineMessage.length() > 300) {
            throw new DomainValidationException("Decline message cannot be longer than 300 characters");
        }

        this.declineMessage = declineMessage;
    }

    public void setReward(Integer reward) {
        if (reward == null) {
            this.reward = null;
            return;
        }

        if (this.reward <= 0) {
            throw new DomainValidationException("Reward must be a positive integer");
        }

        this.reward = reward;
    }

    public void accept() {
        if (this.acceptedAt != null) {
            throw new IllegalStateException("Group task is already accepted");
        }

        this.acceptedAt = Instant.now();
    }

    private void setTaskId(UUID taskId) {
        if (taskId == null) {
            throw new DomainValidationException("Task ID cannot be null");
        }

        this.taskId = taskId;
    }

    private void setGroupId(UUID groupId) {
        if (groupId == null) {
            throw new DomainValidationException("Group ID cannot be null");
        }

        this.groupId = groupId;
    }

    public boolean wasRewardIssued() {
        return this.rewardIssuedAt != null;
    }

    public void markRewardsAsIssued() {
        this.rewardIssuedAt = Instant.now();
    }

    public boolean isRewarded() {
        return this.reward != null;
    }

    public void undoAcceptation() {
        if (!isAccepted()) {
            throw new IllegalStateException("Group task is not accepted");
        }

        this.acceptedAt = null;
    }
}
