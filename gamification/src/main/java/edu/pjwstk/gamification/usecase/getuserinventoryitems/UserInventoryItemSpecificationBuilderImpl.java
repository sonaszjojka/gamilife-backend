package edu.pjwstk.gamification.usecase.getuserinventoryitems;

import edu.pjwstk.gamification.enums.ItemSlotEnum;
import edu.pjwstk.gamification.enums.RarityEnum;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class UserInventoryItemSpecificationBuilderImpl implements UserInventoryItemSpecificationBuilder {

    @Override
    public Specification<UserInventoryItem> buildSpecification(
            UUID userId,
            String itemName,
            ItemSlotEnum itemSlot,
            RarityEnum rarity
    ) {
        return Specification.allOf(
                belongsToUser(userId),
                hasItemName(itemName),
                hasItemSlot(itemSlot),
                hasRarity(rarity)
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