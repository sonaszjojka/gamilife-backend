package pl.gamilife.groupshop.infrastructure.persistence.jpa;

import org.jspecify.annotations.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID>, JpaSpecificationExecutor<OwnedGroupItem> {
    @Override
    @EntityGraph(attributePaths = {"groupItem"})
    Page<OwnedGroupItem> findAll(Specification<OwnedGroupItem> specification, @NonNull Pageable pageable);
}
