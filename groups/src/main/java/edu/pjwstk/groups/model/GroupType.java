package edu.pjwstk.groups.model;

import edu.pjwstk.groups.enums.GroupTypeEnum;
import jakarta.persistence.*;
import lombok.*;

import java.util.List;

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

    @OneToMany(mappedBy = "groupType")
    @ToString.Exclude
    private List<Group> groups;

    public GroupTypeEnum toEnum() {
        return GroupTypeEnum.fromId(this.groupTypeId);
    }
}
