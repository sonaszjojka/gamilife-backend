package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.GroupTypeEnum;
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
@Table(name = "group_type")
public class GroupType {

    @Id
    @Column(name = "group_type_id", nullable = false, updatable = false, unique = true)
    private Integer groupTypeId;

    @Column(name = "title", length = 50, nullable = false, updatable = false)
    private String title;

    @OneToMany(mappedBy = "groupType", fetch = FetchType.LAZY)
    @ToString.Exclude
    private List<Group> groups;

    public GroupTypeEnum toEnum() {
        return GroupTypeEnum.fromId(this.groupTypeId);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        GroupType groupType = (GroupType) o;
        return Objects.equals(groupTypeId, groupType.groupTypeId);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(groupTypeId);
    }
}
