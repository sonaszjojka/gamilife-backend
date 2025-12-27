package pl.gamilife.groupshop.infrastructure.persistence.specification;

import jakarta.validation.constraints.NotNull;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;

import java.util.UUID;

@Component
public class GroupItemSpecificationBuilder {

    public Specification<GroupItem> build(
            GroupItemsFilter filter
    ) {
        return Specification.allOf(
                itemInGroupShop(filter.groupShopId()),
                isActive(filter.isActive())

        );
    }

    private Specification<GroupItem> itemInGroupShop(@NotNull UUID groupShopId) {
        return (root, query, cb) ->

                        {
        if (groupShopId == null) {
            return null;
        }
        return root.get("groupShopId").in(groupShopId);
                       };
    }
    private Specification<GroupItem> isActive(Boolean isActive) {


        return (root, query, cb) ->
        {
            if (isActive == null) {
                return null;
            }
            return root.get("isActive").in(isActive);
        };
    }

}
