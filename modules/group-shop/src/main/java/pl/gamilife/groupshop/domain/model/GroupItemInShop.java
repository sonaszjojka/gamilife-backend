package pl.gamilife.groupshop.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import org.hibernate.annotations.Immutable;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseUuidReadOnlyEntity;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "group_item_in_shop")
@ToString(exclude = {"groupShop","ownedGroupItems"})
public class GroupItemInShop extends BaseUuidReadOnlyEntity {



    @Column(name = "price", nullable = false)
    private Integer price;

    @Column(name = "name", nullable = false, length = 30)
    private String name;

    @Column(name = "is_active", nullable = false)
    private Boolean isActive = false;

    @Column(name = "group_shop_id",nullable = false)
    private UUID groupShopId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_shop_id", nullable = false)
    private GroupShop groupShop;

    @OneToMany(mappedBy = "groupItemInShop")
    private Set<OwnedGroupItem> ownedGroupItems = new LinkedHashSet<>();

    private GroupItemInShop (String name,Integer price,Boolean isActive,GroupShop groupShop)
    {
        setName(name);
        setPrice(price);
        setIsActive(isActive);
        setGroupShop(groupShop);
    }

    public static GroupItemInShop createPrivate(String name,Integer price,Boolean isActive,GroupShop groupShop) {

        return new GroupItemInShop(name,price,isActive,groupShop);

    }

    public void setName(String name)
    {
        if (name == null||name.isBlank())
        {
            throw new DomainValidationException("Item name must be provided");

        }

        if (name.length()>255)
        {
            throw new DomainValidationException("Item name cannot exceed 255 characters");
        }

        this.name = name;

    }

    public void setPrice(Integer price)
    {
        if (price == null)
        {
            throw new DomainValidationException("Price must be provided");

        }
        if (price < 0)
        {
            throw new DomainValidationException("Price cannot be negative");
        }
        if (price > 10000)
        {
            throw new DomainValidationException("Price cannot exceed 10000");
        }

        this.price = price;
    }

    public void setIsActive(Boolean isActive)
    {
        if (isActive == null)
        {
            throw new DomainValidationException("Active status must be provided");
        }

        this.isActive = isActive;
    }

    public void setGroupShop(GroupShop groupShop) {
        if (groupShop == null)
        {
            throw new DomainValidationException("GroupShop must be provided");
        }

        this.groupShop = groupShop;
        this.groupShopId=groupShop.getId();
    }
}
