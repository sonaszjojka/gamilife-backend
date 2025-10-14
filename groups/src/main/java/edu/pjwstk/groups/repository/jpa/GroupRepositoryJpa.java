package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.Group;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupRepositoryJpa extends JpaRepository<Group, UUID> {
}
