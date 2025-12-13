package pl.gamilife.gamification.infrastructure.persistence.specification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;

import java.util.List;

@Component
public class StoreItemSpecificationBuilder {

    public Specification<Item> build(
            StoreItemsFilter filter
    ) {
        return Specification.allOf(
                hasItemName(filter.itemName()),
                hasItemSlot(filter.itemSlotIds()),
                hasRarity(filter.rarityIds()),
                isStoreItem()
        );
    }

    private Specification<Item> hasItemName(String itemName) {
        return (root, query, cb) ->
        {
            if (itemName == null || itemName.isBlank()) {
                return null;
            }
            String searchPattern = "%" + itemName.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("name")), searchPattern);

        };
    }

    private Specification<Item> hasItemSlot(List<Integer> itemSlotIds) {
        return (root, query, cb) -> {
            if (itemSlotIds == null || itemSlotIds.isEmpty()) {
                return null;
            }

            return root.get("itemSlotId").in(itemSlotIds);
        };
    }

    private Specification<Item> hasRarity(List<Integer> rarityIds) {
        return (root, query, cb) ->
        {
            if (rarityIds == null || rarityIds.isEmpty()) {
                return null;
            }

            return root.get("rarityId").in(rarityIds);
        };
    }

    private Specification<Item> isStoreItem() {
        return (root, query, cb) ->
                cb.isNotNull(root.get("price"));
    }

}
