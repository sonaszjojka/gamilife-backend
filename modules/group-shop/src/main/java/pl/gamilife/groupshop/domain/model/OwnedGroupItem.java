package pl.gamilife.groupshop.domain.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@ToString(exclude = {"groupItem"})
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "owned_group_item", schema = "group_shop")
public class OwnedGroupItem extends BaseEntity {

    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @Column(name = "group_item_id", nullable = false, insertable = false, updatable = false)
    private UUID groupItemInShopId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_item_id", nullable = false)
    private GroupItem groupItem;


    @Column(name = "used_at")
    private Instant usedAt;

    private OwnedGroupItem(UUID groupMemberId, GroupItem groupItem) {

        setGroupMemberId(groupMemberId);
        setGroupItem(groupItem);
        useItem(false);
    }

    public static OwnedGroupItem createPrivate(UUID groupMemberId, GroupItem groupItem) {

        return new OwnedGroupItem(groupMemberId, groupItem);

    }

    public void setGroupMemberId(UUID groupMemberId) {
        if (groupMemberId == null) {
            throw new DomainValidationException("Group Member Id cannot be null");
        }
        this.groupMemberId = groupMemberId;
    }

    public void setGroupItem(GroupItem groupItem) {

        if (groupItem == null) {
            throw new DomainValidationException("Group Item In Shop cannot be null");
        }

        this.groupItem = groupItem;
        this.groupItemInShopId = groupItem.getId();

    }

    public void useItem(Boolean isUsedUp) {

        if (isUsedUp != null && isUsedUp) {
            this.usedAt = Instant.now();
        } else {
            this.usedAt = null;
        }


    }


}
