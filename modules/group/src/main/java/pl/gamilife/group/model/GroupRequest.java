package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_request")
public class GroupRequest extends BaseEntity {

    @Column(name = "user_id", nullable = false, updatable = false)
    private UUID userId;

    @Column(name = "group_id", nullable = false, updatable = false, insertable = false)
    private UUID groupId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    @ToString.Exclude
    private Group group;

    @Column(name = "status_id", nullable = false, updatable = false, insertable = false)
    private Integer statusId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "status_id", nullable = false)
    private GroupRequestStatus status;

    private GroupRequest(Group group, UUID userId, GroupRequestStatus status) {
        setGroup(group);
        setUserId(userId);
        changeStatus(status);
    }

    public static GroupRequest create(Group group, UUID userId, GroupRequestStatus status) {
        return new GroupRequest(group, userId, status);
    }

    public boolean hasStatus(GroupRequestStatusEnum statusEnum) {
        return this.status.toEnum() == statusEnum;
    }

    public boolean belongsToUser(UUID userId) {
        return this.userId.equals(userId);
    }

    public void changeStatus(GroupRequestStatus groupRequestStatus) {
        if (groupRequestStatus == null) {
            throw new DomainValidationException("Group request status cannot be null");
        }

        this.status = groupRequestStatus;
        this.statusId = groupRequestStatus.getId();
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

}
