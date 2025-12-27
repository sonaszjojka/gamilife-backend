package pl.gamilife.groupshop.infrastructure.persistence.jpa;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID> {

@EntityGraph(attributePaths = {"groupItem",})
Page<OwnedGroupItem> findAllMemberItems(Specification<OwnedGroupItem> specification, Pageable pageable);
}
