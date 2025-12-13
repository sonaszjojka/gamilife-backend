package pl.gamilife.groupshop.domain.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_shop")
@ToString
public class GroupShop extends BaseEntity {
    @Column(name = "name", nullable = false, length = 100)
    private String name;

    @Column(name = "description", nullable = false, length = 500)
    private String description;


    @Column(name = "group_id", nullable = false)
    private UUID groupId;


    @Column(name = "is_active", nullable = false)
    private Boolean isActive;

    @OneToMany(mappedBy = "groupShop")
    @ToString.Exclude
    private Set<GroupItemInShop> groupItemInShops = new LinkedHashSet<>();


    private GroupShop(String name, String description, UUID groupId, Boolean isActive) {

        setName(name);
        setDescription(description);
        setGroupId(groupId);
        setIsActive(isActive);
    }

    public static GroupShop createForGroup(String name, String description, UUID groupId)
    {
        return new GroupShop(name, description, groupId, true);
    }

    public void setName(String name)
    {
        if (name == null||name.isBlank())
        {
            throw new DomainValidationException("Name cannot be null or blank");
        }
        this.name = name;
    }

    public void setGroupId(UUID groupId)
    {
        if (groupId == null)
        {
            throw new DomainValidationException("Group Id must be provided");
        }
    }

    public void setDescription(String description)
    {
        if (description == null||description.isBlank())
        {
            throw new DomainValidationException("Description cannot be null or blank");
        }

    }

    public void setIsActive(Boolean isActive)
    {
        if (isActive == null)
        {
            throw new DomainValidationException("Active status cannot be null");
        }
    }



}
