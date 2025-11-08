package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.Group;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.UUID;

public interface GroupJpaRepository extends JpaRepository<Group, UUID>, JpaSpecificationExecutor<Group> {
}
