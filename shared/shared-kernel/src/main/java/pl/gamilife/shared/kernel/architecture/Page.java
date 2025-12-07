package pl.gamilife.shared.kernel.architecture;

import java.util.List;
import java.util.function.Function;

public record Page<T>(
        List<T> content,
        long totalElements,
        int totalPages,
        int number,
        int size
) {
    public <R> Page<R> map(Function<T, R> mapper) {
        return new Page<>(
                content.stream().map(mapper).toList(),
                totalElements,
                totalPages,
                number,
                size
        );
    }
}