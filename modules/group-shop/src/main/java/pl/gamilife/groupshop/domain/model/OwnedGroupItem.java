package pl.gamilife.groupshop.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@ToString(exclude = {"group_item_in_shop_id"})
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "owned_group_item")
public class OwnedGroupItem extends BaseEntity {

    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @Column(name = "group_item_in_shop_id", nullable = false)
    private UUID groupItemInShopId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_item_in_shop_id", nullable = false)
    private GroupItemInShop groupItemInShop;


    @Column(name = "is_used_up", nullable = false)
    private Boolean isUsedUp = false;

    @Column(name = "use_date")
    private Instant useDate;

    private OwnedGroupItem(UUID groupMemberId, GroupItemInShop groupItemInShop) {

        setGroupMemberId(groupMemberId);
        setGroupItemInShop(groupItemInShop);
        setIsUsedUp(false);


    }

    public static OwnedGroupItem createPrivate(UUID groupMemberId, GroupItemInShop groupItemInShop) {

        return new OwnedGroupItem(groupMemberId, groupItemInShop);

    }

    public void setGroupMemberId(UUID groupMemberId)
    {
        if (groupMemberId == null) {
            throw new DomainValidationException("Group Member Id cannot be null");
        }
        this.groupMemberId = groupMemberId;
    }

    public void setGroupItemInShop(GroupItemInShop groupItemInShop) {

        if (groupItemInShop == null) {
            throw new DomainValidationException("Group Item In Shop cannot be null");
        }

        this.groupItemInShop = groupItemInShop;
        this.groupItemInShopId = groupItemInShop.getId();

    }

    public void setIsUsedUp(Boolean isUsedUp) {
        if (isUsedUp == null) {
            throw new DomainValidationException("IsUsedUp cannot be null");
        }
        this.isUsedUp = isUsedUp;
        this.useItem(isUsedUp);
    }

    public void useItem(Boolean isUsedUp) {

       if (isUsedUp!=null && isUsedUp ) {
           this.useDate=Instant.now();
       }
       else  {
           this.useDate=null;
       }


    }


}
