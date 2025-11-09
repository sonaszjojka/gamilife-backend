package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_request")
public class GroupRequest {

    @Id
    @Column(name = "group_request_id", nullable = false, updatable = false, unique = true)
    private UUID groupRequestId;

    @Column(name = "user_id", nullable = false, updatable = false)
    private UUID userId;

    @Column(name = "created_at", nullable = false, updatable = false)
    protected Instant createdAt;

    @Column(name = "group_id", nullable = false, updatable = false, insertable = false)
    private UUID groupId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    @ToString.Exclude
    private Group groupRequested;

    @Column(name = "status_id", nullable = false, updatable = false, insertable = false)
    private UUID groupRequestStatusId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "status_id", nullable = false)
    private GroupRequestStatus groupRequestStatus;

    @PrePersist
    public void prePersist() {
        if (this.createdAt == null) {
            this.createdAt = Instant.now();
        }
    }

    public boolean hasStatus(GroupRequestStatusEnum statusEnum) {
        return this.groupRequestStatus.toEnum() == statusEnum;
    }

    public boolean belongsToUser(UUID userId) {
        return this.userId.equals(userId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        GroupRequest that = (GroupRequest) o;
        return Objects.equals(groupRequestId, that.groupRequestId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupRequestId);
    }
}
