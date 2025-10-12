package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.domain.GroupRequest;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupRequestRepositoryJpa extends JpaRepository<GroupRequest, UUID> {
}
