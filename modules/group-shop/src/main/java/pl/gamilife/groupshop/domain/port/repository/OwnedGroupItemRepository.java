package pl.gamilife.groupshop.domain.port.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.filter.OwnedGroupItemsFilter;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

public interface OwnedGroupItemRepository {

    OwnedGroupItem save(OwnedGroupItem ownedGroupItem);

    void deleteById(UUID ownedGroupItemId);

    Optional<OwnedGroupItem> findById(UUID ownedGroupItemId);

    Page<OwnedGroupItem> findAllMemberItems(OwnedGroupItemsFilter filter, Integer page, Integer size);

    Optional<OwnedGroupItem> findWithGroupItemById(@NotNull UUID ownedGroupItemId);
}
