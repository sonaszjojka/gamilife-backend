package edu.pjwstk.groupshop.repository.jpa;

import edu.pjwstk.groupshop.entity.OwnedGroupItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID> {
}
