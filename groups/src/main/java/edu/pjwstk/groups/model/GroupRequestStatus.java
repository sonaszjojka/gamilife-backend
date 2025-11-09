package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import jakarta.persistence.*;
import lombok.*;

import java.util.List;
import java.util.Objects;


@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_request_status")
public class GroupRequestStatus {

    @Id
    @Column(name = "group_request_status_id", nullable = false, updatable = false, unique = true)
    private Integer groupRequestStatusId;

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "groupRequestStatus", fetch = FetchType.LAZY)
    @ToString.Exclude
    private List<GroupRequest> groupRequests;

    public GroupRequestStatusEnum toEnum() {
        return GroupRequestStatusEnum.fromId(this.groupRequestStatusId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        GroupRequestStatus that = (GroupRequestStatus) o;
        return Objects.equals(groupRequestStatusId, that.groupRequestStatusId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupRequestStatusId);
    }
}
