package pl.gamilife.grouptask.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_task_member", schema = "group_task")
public class GroupTaskMember extends BaseEntity {

    @Column(name = "group_task_id", nullable = false, insertable = false, updatable = false)
    private UUID groupTaskId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_task_id", nullable = false)
    @ToString.Exclude
    private GroupTask groupTask;

    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @Column(name = "marked_done_at")
    private Instant markedDoneAt;

    private GroupTaskMember(GroupTask groupTask, UUID groupMemberId) {
        setGroupTask(groupTask);
        setGroupMemberId(groupMemberId);
    }

    public static GroupTaskMember create(GroupTask groupTask, UUID groupMemberId) {
        return new GroupTaskMember(groupTask, groupMemberId);
    }

    public boolean isMarkedDone() {
        return this.markedDoneAt != null;
    }

    public void changeDoneStatus(boolean done) {
        if (done && this.markedDoneAt != null) {
            throw new IllegalStateException("Group task member is already marked as done");
        }

        if (!done && this.markedDoneAt == null) {
            throw new IllegalStateException("Group task member is already marked as not done");
        }

        this.markedDoneAt = done ? Instant.now() : null;
    }

    private void setGroupTask(GroupTask groupTask) {
        if (groupTask == null) {
            throw new DomainValidationException("Group task cannot be null");
        }

        this.groupTask = groupTask;
        this.groupTaskId = groupTask.getId();
    }

    private void setGroupMemberId(UUID groupMemberId) {
        if (groupMemberId == null) {
            throw new DomainValidationException("Group member ID cannot be null");
        }

        this.groupMemberId = groupMemberId;
    }

}
