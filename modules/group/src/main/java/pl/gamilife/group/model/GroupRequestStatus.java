package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.List;


@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_request_status")
public class GroupRequestStatus extends BaseIntReadOnlyEntity {

    @Column(name = "title", length = 100, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "status", fetch = FetchType.LAZY)
    @ToString.Exclude
    private List<GroupRequest> groupRequests;

    public GroupRequestStatusEnum toEnum() {
        return GroupRequestStatusEnum.fromId(this.getId());
    }
}
