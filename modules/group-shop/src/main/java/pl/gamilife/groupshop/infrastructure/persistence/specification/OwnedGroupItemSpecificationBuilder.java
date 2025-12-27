package pl.gamilife.groupshop.infrastructure.persistence.specification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.filter.OwnedGroupItemsFilter;

import java.util.UUID;

@Component
public class OwnedGroupItemSpecificationBuilder {

    public Specification<OwnedGroupItem> build(
            OwnedGroupItemsFilter filter
    ){
        return Specification.allOf(
                belongsTo(filter.memberId()),
                isUsedUp(filter.isUsedUp())
        );
    }

    private Specification<OwnedGroupItem> belongsTo(
            UUID memberId
    ){
        return (root, query, cb) ->
                cb.equal(
                        root.get("memberId"),
                        memberId
                );
    }

    private Specification<OwnedGroupItem> isUsedUp(
            Boolean isUsedUp
    ){
        if(isUsedUp == null){
            return null;
        }
        return (root, query, cb) ->

                cb.equal(
                        root.get("isUsedUp"),
                        isUsedUp
                );
    }
}
