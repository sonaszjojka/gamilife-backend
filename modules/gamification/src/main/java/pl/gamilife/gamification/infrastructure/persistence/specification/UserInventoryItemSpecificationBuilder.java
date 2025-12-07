package pl.gamilife.gamification.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.UserInventoryItemFilter;

import java.util.UUID;

@Component
public class UserInventoryItemSpecificationBuilder {

    public Specification<UserInventoryItem> build(
            UserInventoryItemFilter filter
    ) {
        return Specification.allOf(
                belongsToUser(filter.userId()),
                hasItemName(filter.itemName()),
                hasItemSlot(filter.itemSlot()),
                hasRarity(filter.rarity())
        );
    }

    private Specification<UserInventoryItem> belongsToUser(UUID userId) {
        return (root, query, cb) -> {
            if (userId == null) {
                return null;
            }
            return cb.equal(root.get("userId"), userId);
        };
    }

    private Specification<UserInventoryItem> hasItemName(String itemName) {
        return (root, query, cb) -> {
            if (itemName == null || itemName.isBlank()) {
                return null;
            }
            Join<UserInventoryItem, Item> itemJoin = root.join("item");
            String searchPattern = "%" + itemName.trim().toLowerCase() + "%";
            return cb.like(cb.lower(itemJoin.get("name")), searchPattern);
        };
    }

    private Specification<UserInventoryItem> hasItemSlot(ItemSlotEnum itemSlot) {
        return (root, query, cb) -> {
            if (itemSlot == null) {
                return null;
            }
            Join<UserInventoryItem, Item> itemJoin = root.join("item");
            return cb.equal(itemJoin.get("itemSlotId"), itemSlot.getItemSlotId());
        };
    }

    private Specification<UserInventoryItem> hasRarity(RarityEnum rarity) {
        return (root, query, cb) -> {
            if (rarity == null) {
                return null;
            }
            Join<UserInventoryItem, Item> itemJoin = root.join("item");
            return cb.equal(itemJoin.get("rarityId"), rarity.getRarityId());
        };
    }
}