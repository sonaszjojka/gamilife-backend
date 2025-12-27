package pl.gamilife.group.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_type", schema = "group")
public class GroupType extends BaseIntReadOnlyEntity {

    @Column(name = "title", length = 50, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "type", fetch = FetchType.LAZY)
    @ToString.Exclude
    private List<Group> groups;

    public GroupTypeEnum toEnum() {
        return GroupTypeEnum.fromId(this.getId());
    }

}
