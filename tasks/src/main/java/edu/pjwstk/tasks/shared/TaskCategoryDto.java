package edu.pjwstk.tasks.shared;

import java.io.Serializable;

/**
 * DTO for {@link edu.pjwstk.tasks.entity.TaskCategory}
 */
public record TaskCategoryDto(Integer id) implements Serializable {
}